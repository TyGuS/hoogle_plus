{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module PetriNet.PNSolver (runPNSolver) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Lens
import Control.Monad.State
import Control.Monad.Logic
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Data (Data)
import qualified Data.Char as Char
import Data.Either hiding (fromLeft, fromRight)
import Data.List.Extra
import Language.Haskell.Exts.Parser (parseExp, ParseResult(..))
import Control.Concurrent.Chan
import Data.Tuple
import Text.Printf
import Debug.Trace

import Types.Common
import Types.Type
import Types.Environment
import Types.Abstract
import Types.Solver
import Types.Program
import Types.Experiments
import Types.Encoder hiding (varName, mustFirers, incrementalSolving)
import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Program
import Synquid.Type
import Synquid.Logic hiding (varName)
import Synquid.Util
import Synquid.Error
import Synquid.Pretty
import PetriNet.AbstractType
import PetriNet.PNEncoder
import PetriNet.GHCChecker
import PetriNet.Util
import HooglePlus.Stats
import HooglePlus.CodeFormer
import HooglePlus.Abstraction
import HooglePlus.Refinement
import Database.Convert
import Database.Generate

encodeFunction :: Id -> AbstractSkeleton -> FunctionCode
encodeFunction id t | pairProj `isPrefixOf` id = 
    let toMatch (FunctionCode name ho [p1,p2] ret) = FunctionCode id ho [p1] (p2:ret)
     in toMatch $ encodeFunction "__f" t
encodeFunction id t@(AFunctionT tArg tRet) = FunctionCode id hoParams params [lastAbstract t]
  where
    base = (0, [])
    hoFun x = encodeFunction (show x) x
    hoParams = map hoFun $ filter isAFunctionT (abstractParamList t)
    params = abstractParamList t
encodeFunction id t@AScalar {} = FunctionCode id [] [] [t]

instantiate :: MonadIO m => Environment -> Map Id RSchema -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs = do
    modify $ set toRemove []
    Map.fromList <$> instantiate' sigs
  where
    instantiate' sigs = do
        tree <- gets (view abstractionCover)
        let typs = allTypesOf tree
        writeLog 2 "instantiate" $ text "Current abstract types:" <+> text (show tree)
        sigs' <- Map.toList <$> mapM freshType sigs
        foldM (\acc -> (<$>) (acc ++) . uncurry (instantiateWith env typs)) [] sigs'

-- add Pair_match function as needed
instantiateWith :: MonadIO m => Environment -> [AbstractSkeleton] -> Id -> RType -> PNSolver m [(Id, AbstractSkeleton)]
-- skip "snd" function, it would be handled together with "fst"
instantiateWith env typs id t | id == "snd" = return []
instantiateWith env typs id t = do
    instMap <- gets (view instanceMapping)
    let bound = env ^. boundTypeVars
    if id == "fst"
       then do -- this is hack, hope to get rid of it sometime
           first <- freshType (Monotype t)
           secod <- findSymbol env "snd"
           fstSigs <- enumSigs $ toAbstractType $ shape first
           sndSigs <- enumSigs $ toAbstractType $ shape secod
           -- assertion, check they have same elements
           when (length fstSigs /= length sndSigs)
                (error "fst and snd have different number of instantiations")
           let matches = zipWith assemblePair fstSigs sndSigs
           let matches' = filter (\t -> noneInst instMap pairProj t || diffInst bound instMap pairProj t) matches
           mapM (mkNewSig pairProj) matches'
       else do
           ft <- freshType (Monotype t)
           let t' = toAbstractType (shape ft)
           rawSigs <- enumSigs t'
           let sigs = filter (\t -> noneInst instMap id t || diffInst bound instMap id t) rawSigs
           mapM (mkNewSig id) sigs
  where
    enumSigs typ = do
        let bound = env ^. boundTypeVars
        let argNum = length (decompose typ) - 1
        let allArgCombs = multiPermutation argNum typs
        applyRes <- mapM (applySemantic bound typ) allArgCombs
        let resSigs = filter (not . isBot . fst) (zip applyRes allArgCombs)
        return $ map (uncurry $ foldr AFunctionT) resSigs

mkNewSig :: MonadIO m => Id -> AbstractSkeleton -> PNSolver m (Id, AbstractSkeleton)
mkNewSig id ty = do
    instMap <- gets (view instanceMapping)
    newId <- if pairProj `isPrefixOf` id then freshId (id ++ "_")
                                         else freshId "f"
    -- when same arguments exist for a function, replace it
    unless (noneInst instMap id ty) (excludeUseless id ty)
    modify $ over nameMapping (Map.insert newId id)
    modify $ over instanceMapping (HashMap.insert (id, absFunArgs id ty) (newId, ty))
    writeLog 3 "mkNewSig" $ text id <+> text "==>"  <+> text newId <+> text "::" <+> pretty ty
    return (newId, ty)

splitTransition :: MonadIO m => Environment -> AbstractSkeleton -> Id -> PNSolver m [(Id, AbstractSkeleton)]
splitTransition env newAt fid = do
    rep <- head <$> getGroupRep fid
    sigs <- gets $ view currentSigs
    let ty = lookupWithError "currentSigs" rep sigs
    writeLog 3 "splitTransition" $ text "split transtion" <+> text fid <+> text "::" <+> pretty ty
    allSubsts ty
  where
    allSubsts ty = do
        funcs <- gets $ view funTypes
        writeLog 3 "allSubsts" $ text "current fun types:" <+> pretty (Set.toList funcs)
        if fid `Set.member` funcs then return []
                                  else allSubsts' (lastAbstract ty) (absFunArgs fid ty)

    allSubsts' ret args | pairProj `isPrefixOf` fid = do
        cover <- gets $ view abstractionCover
        nameMap <- gets $ view nameMapping
        let parents = HashMap.keys $ HashMap.filter (Set.member newAt) cover
        let args' = enumArgs parents (take 1 args)
        let fstRet = args !! 1
        let sndRet = ret
        fstType <- findSymbol env "fst"
        sndType <- findSymbol env "snd"
        let first = toAbstractType $ shape fstType
        let secod = toAbstractType $ shape sndType
        let tvs = env ^. boundTypeVars
        fstRets <- mapM (applySemantic tvs first) args'
        sndRets <- mapM (applySemantic tvs secod) args'
        let sameFunc ([a], (fret, sret)) = equalAbstract tvs fret fstRet 
                                         && equalAbstract tvs sret sndRet
                                         && a == head args
        let validFunc (a, (fret, sret)) = not (sameFunc (a, (fret, sret)))
                                      && not (isBot fret)
                                      && not (isBot sret)
        let funcs = filter validFunc (zip args' (zip fstRets sndRets))
        let sigs = map (\([a], (ft, st)) -> assemblePair (AFunctionT a ft) (AFunctionT a st)) funcs
        mapM (mkNewSig pairProj) sigs
    allSubsts' ret args = do
        cover <- gets $ view abstractionCover
        nameMap <- gets $ view nameMapping
        splits <- gets $ view splitTypes
        let parents = HashMap.keys $ HashMap.filter (Set.member newAt) cover
        -- let children = HashMap.lookupDefault Set.empty newAt cover `Set.difference` splits
        -- let substedTyps = parents ++ Set.toList children
        let args' = enumArgs parents args
        writeLog 3 "allSubsts'" $ pretty args'
        funType <- findSymbol env fid
        writeLog 3 "allSubsts'" $ text fid <+> text "::" <+> pretty funType
        let absFunType = toAbstractType (shape funType)
        let tvs = env ^. boundTypeVars
        rets <- mapM (applySemantic tvs absFunType) args'
        writeLog 3 "allSubsts'" $ text fid <+> text "returns" <+> pretty rets
        let validFunc (a, r) = not (equalAbstract tvs ret r && a == args) && not (isBot r)
        let funcs = filter validFunc (zip args' rets)
        let sigs = map (\f -> foldr AFunctionT (snd f) (fst f)) funcs
        let funId = lookupWithError "nameMapping" fid nameMap
        mapM (mkNewSig funId) sigs
        
    enumArgs parents [] = [[]]
    enumArgs parents (arg:args)
      | arg `elem` parents = let args' = enumArgs parents args 
                              in [a:as | a <- [arg, newAt], as <- args']
      | otherwise = map (arg:) (enumArgs parents args)

    
addHoArg :: MonadIO m => Environment -> Map Id AbstractSkeleton -> Bool -> Id -> PNSolver m (Map Id AbstractSkeleton)
addHoArg env sigs queryArg id = do
    nameMap <- gets $ view nameMapping
    funcs <- gets $ view funTypes
    let isHoInstance k n = id == n && not (k `Set.member` funcs)
    let names = Map.keys $ Map.filterWithKey isHoInstance nameMap
    sigMbs <- mapM addOrRemoveHo names
    let sigs' = Map.fromList $ map fromJust $ filter isJust sigMbs
    removes <- gets $ view toRemove
    return $ Map.withoutKeys sigs' $ Set.fromList removes
  where
    addOrRemoveHo name = do
      writeLog 3 "addHoArg" $ text "checking" <+> text name <+> text "for" <+> text id
      instMap <- gets $ view instanceMapping
      cover <- gets $ view abstractionCover
      currSigs <- gets $ view currentSigs
      let tvs = env ^. boundTypeVars
      let sigs' = currSigs `Map.union` sigs
      let mkCompact = currentAbst tvs cover . compactAbstractType . fromJust
      groupName <- getGroupRep name
      let name' = if null groupName then name else head groupName
      t <- mkCompact (Map.lookup name' sigs')
      writeLog 3 "addHoArg" $ text "checking" <+> text name <+> text "::" <+> pretty t
      envTyp <- findSymbol env id
      let absEnvTyp = compactAbstractType $ toAbstractType $ shape envTyp
      if isSubtypeOf tvs absEnvTyp t &&
         (noneInst instMap id t || diffInst tvs instMap id t)
        then do
          unless (noneInst instMap id t) (excludeUseless id t)
          f <- freshId "f"
          let args = absFunArgs id t
          modify $ over funTypes (Set.insert f)
          modify $ over nameMapping $ Map.insert f id
          modify $ over instanceMapping (HashMap.insert (id, args) (f, t))
          writeLog 3 "addOrRemoveHo" $ text "add higher order transition" <+> text f <+> text "::" <+> pretty t
          return $ Just (f, t)
        else return Nothing

addSignatures :: MonadIO m => Environment -> PNSolver m (Map Id AbstractSkeleton)
addSignatures env = do
    let foArgs = Map.keys $ foArgsOf env
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    let envSymbols = allSymbols env
    let usefulPipe k _ = k `notElem` foArgs
    let usefulSymbols = Map.filterWithKey usefulPipe envSymbols
    let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let binds = env ^. boundTypeVars
    cands <- getExperiment hoCandidates
    envSigs <- instantiate env usefulSymbols
    -- add higher order functions
    hoSigs <- mapM (addHoArg env envSigs False) cands
    hoSigs' <- mapM (addHoArg env envSigs True) $ Map.keys hoArgs
    let sigs = Map.unions $ envSigs : hoSigs ++ hoSigs'

    sigs' <- ifM (getExperiment coalesceTypes) (mkGroups env sigs) (return sigs)
    modify $ over activeSigs (Set.union (Map.keysSet sigs'))
    mapM_ addEncodedFunction (Map.toList sigs')
    return sigs'

mkGroups :: MonadIO m => Environment -> Map Id AbstractSkeleton -> PNSolver m (Map Id AbstractSkeleton)
mkGroups env sigs = do
    let encodedSigs = Map.mapWithKey encodeFunction sigs
    (t2g, sigGroups) <- groupSignatures encodedSigs
    -- Do I want to take the sigs here?
    nameMap <- gets $ view nameMapping
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let hoAlias = Map.keysSet $ Map.filter (`elem` hoArgs) nameMap
    let representatives = Map.map (selectRepresentative hoAlias) sigGroups
    let sigs' = Map.restrictKeys sigs (Set.fromList $ Map.elems representatives)
    modify $ over groupRepresentative $ Map.union representatives
    modify $ over typeToGroup (Map.union t2g)
    mapM_ updateGroups (Map.toList sigGroups)
    return sigs'
  where
    -- Given the new set of groups, update the current group mapping
    -- updateGroups :: Map Id (Set Id) -> Map Id (Set Id) -> Map Id (Set Id)
    -- updateGroups sigGroups gm = foldr updategm gm (Map.toList sigGroups)
    -- updategm (groupName, groupMembers) = Map.insertWith Set.union groupName groupMembers
    updateGroups (gid, fids) = do
        modify $ over groupMap (Map.insertWith Set.union gid fids)
        mapM_ (\fid -> modify $ over nameToGroup (Map.insert fid gid)) fids 

selectRepresentative hoArgs s = 
    let intsct = s `Set.intersection` hoArgs
     in if not (Set.null intsct) then Set.elemAt 0 intsct
                                 else Set.elemAt 0 s

addMusters :: MonadIO m => Id -> PNSolver m ()
addMusters arg = do
    nameMap <- gets $ view nameMapping
    let argFuncs = Map.keys $ Map.filter (arg ==) nameMap
    argRps <- mapM getGroupRep argFuncs
    modify $ over mustFirers (HashMap.insert arg $ concat argRps)

-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m SplitInfo
refineSemantic env prog at = do
    cover <- gets $ view abstractionCover
    writeLog 2 "instantiate" $ text "Current abstract types:" <+> text (show cover)
    -- back propagation of the error types to get all split information
    propagate env prog $ compactAbstractType at
    -- get the split pairs
    splits <- gets (view splitTypes)
    let tvs = env ^. boundTypeVars
    let sortedSplits = reverse $ sortBy (compareAbstract tvs) (Set.toList splits)
    writeLog 3 "refineSemantic splitTypes" $ pretty sortedSplits
    -- get removed transitions
    addsAndRemoves <- mapM splitTransitions sortedSplits
    let (adds, removes) = unzip addsAndRemoves
    let addedSigs = map fst (concat adds)
    let removedSigs = concat removes
    let toAdd = addedSigs \\ removedSigs
    let removables = removedSigs \\ addedSigs
    writeLog 3 "refineSemantic (toAdd, removables)" $ pretty (toAdd, removables)

    -- update the higer order query arguments
    let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    mapM_ addMusters (Map.keys hoArgs)
    -- call the refined encoder on these signatures and disabled signatures
    return SplitInfo { newPlaces = Set.toList splits
                     , removedTrans = removables
                     , newTrans = addedSigs
                     }
  where
    splitTransitions at = do
        modify $ set toRemove []

        cover <- gets $ view abstractionCover
        t2tr <- gets $ view type2transition
        gm <- gets $ view groupMap
        gr <- gets $ view groupRepresentative
        let parents = HashMap.keys $ HashMap.filter (Set.member at) cover
        let pids = Set.unions $ map (\p -> HashMap.lookupDefault Set.empty p t2tr) parents
        let gids = Map.keys $ Map.filter (`Set.member` pids) gr
        let fids = concatMap (\gid -> Set.toList $ Map.findWithDefault Set.empty gid gm) gids
        sigs <- mapM (splitTransition env at) fids
        let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
        let tvs = env ^. boundTypeVars
        let envSigs = Map.fromList (concat sigs)
        cands <- getExperiment hoCandidates
        hoSigs <- mapM (addHoArg env envSigs False) cands
        hoSigs' <- mapM (addHoArg env envSigs True) $ Map.keys hoArgs
        let allSigs = concat $ sigs ++ map Map.toList (hoSigs ++ hoSigs')
        sigs' <- mkGroups env $ Map.fromList allSigs
        mapM_ addEncodedFunction (Map.toList sigs')

        -- update the group information by the current toRemove
        toCoalesce <- getExperiment coalesceTypes 
        (toAdd, removables) <- changeGroups toCoalesce    
        writeLog 3 "splitTransitions (toAdd, removables)" $ pretty (toAdd, removables)
        mapM_ addEncodedFunction toAdd

        let adds = Map.toList sigs' ++ toAdd
        modify $ over activeSigs (Set.union $ Set.fromList $ map fst adds)
        modify $ over activeSigs (`Set.difference` Set.fromList removables)
        modify $ over mustFirers (HashMap.map (filter (`notElem` removables)))
        modify $ over funTypes (`Set.difference` Set.fromList removables)
        modify $ over nameMapping (`Map.withoutKeys` Set.fromList removables)
        return (adds, removables)

    changeGroups True = do
        removables' <- gets $ view toRemove
        writeLog 3 "changeGroups" $ pretty removables'
        splitGroups removables'
    changeGroups False = do
        removables <- gets $ view toRemove
        return ([], removables)

    splitGroups :: MonadIO m => [Id] -> PNSolver m ([(Id, AbstractSkeleton)], [Id])
    splitGroups removables = do
        -- Step 1: modify groupmap to exclude all those in removables
        -- Some groups may no longer exist as a result of this operation.
        -- Some groups representative may not longer be valid.
        -- TODO: This operation could be slow. find a way to go from id -> groupId (or id -> abstrTy)
        gm <- gets $ view groupMap
        let gm' = Map.fromList  $ map (\(a,b) -> (a, fromJust b))
                              $ filter (isJust . snd)
                              $ map (\(k, vs) -> (k, shrinkSet (Set.fromList removables) vs))
                              $ Map.toList gm
        modify $ set groupMap gm'
        grs <- gets $ view groupRepresentative
        -- Step 2: fixup the group representatives, while also deciding what we can safely remove,
        -- and what we need to add (new representatives).
        (toAdd, toRemove, grs') <- foldM updateRepresentatives ([], [], Map.empty) (Map.toList grs)
        modify $ set groupRepresentative grs'
        mapM_ (\t -> modify $ over type2transition (HashMap.map $ Set.delete t)) removables
        let removeSet = Set.fromList removables
        modify $ over nameMapping (`Map.withoutKeys` removeSet)
        modify $ over currentSigs (`Map.withoutKeys` removeSet)
        modify $ over nameToGroup (`Map.withoutKeys` removeSet)
        return (toAdd, toRemove)

    updateRepresentatives :: MonadIO m => ([(Id, AbstractSkeleton)], [Id], Map GroupId Id) -> (GroupId, Id) -> PNSolver m ([(Id, AbstractSkeleton)], [Id], Map GroupId Id)
    updateRepresentatives (addables, removables, newReps) (gid, rep)= do
        gm <- gets $ view groupMap
        sigs <- gets $ view currentSigs
        nameMap <- gets $ view nameMapping
        let mbCurrentGroup = Map.lookup gid gm
        case mbCurrentGroup of
          -- The group was eliminated entirely by the removables
          Nothing -> return (addables, rep:removables, newReps)
          Just currentGroup ->
            if rep `Set.notMember` currentGroup
                then do
                    let removables' = rep:removables
                    if not (null currentGroup)
                      then do
                        -- We need to elect a new leader for the group!
                        let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
                        let hoAlias = Map.keysSet $ Map.filter (`elem` hoArgs) nameMap
                        let newRep = selectRepresentative hoAlias currentGroup
                        let abstractType = lookupWithError "currentSigs" rep sigs
                        let addables' = (newRep, abstractType):addables
                        return (addables', removables', Map.insert gid newRep newReps)
                      -- There was only that one element in the group, so with the leader gone, the group goes away.
                      else do
                        return (addables, removables', newReps)
                -- after all the removals, the current representative is still in its original group
                else return (addables, removables, Map.insert gid rep newReps)

    shrinkSet :: Set Id -> Set Id -> Maybe (Set Id)
    shrinkSet toRemove ids = let
        ids' = Set.difference ids toRemove
        in if Set.null ids' then Nothing else Just ids'


initNet :: MonadIO m => Environment -> PNSolver m ()
initNet env = withTime ConstructionTime $ do
    -- reset the solver state
    modify $ set functionMap HashMap.empty
    modify $ set currentSigs Map.empty
    modify $ set type2transition HashMap.empty
    modify $ set instanceMapping HashMap.empty

    addSignatures env
    -- add higher order query arguments
    let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    mapM_ addMusters (Map.keys hoArgs)
  where
    abstractSymbol id sch = do
        t <- freshType sch
        let absTy = toAbstractType (shape t)
        return (id, absTy)

addEncodedFunction :: MonadIO m => (Id, AbstractSkeleton) -> PNSolver m ()
addEncodedFunction (id, f) = do
    let ef = encodeFunction id f
    modify $ over functionMap (HashMap.insert id ef)
    modify $ over currentSigs (Map.insert id f)
    -- store the used abstract types and their groups into mapping
    updateTy2Tr id f

resetEncoder :: (MonadIO m) => Environment -> RType -> PNSolver m EncodeState
resetEncoder env dst = do
    (srcTypes, tgt) <- updateSrcTgt env dst
    writeLog 2 "resetEncoder" $ text "parameter types are" <+> pretty srcTypes
    writeLog 2 "resetEncoder" $ text "return type is" <+> pretty tgt
    (loc, musters, rets, funcs, tid2tr, incremental) <- prepEncoderArgs env tgt
    liftIO $ encoderInit loc musters srcTypes rets funcs tid2tr incremental

incEncoder :: MonadIO m => Environment -> EncodeState -> PNSolver m EncodeState
incEncoder env st = do
    tgt <- gets (view targetType)
    src <- gets (view sourceTypes)
    (_, _, rets, funcs, _, _) <- prepEncoderArgs env tgt
    liftIO $ execStateT (encoderInc funcs src rets) st

findPath :: MonadIO m 
         => Environment 
         -> RType 
         -> EncodeState 
         -> PNSolver m ([Id], EncodeState)
findPath env dst st = do
    (res, st') <- withTime SolverTime (liftIO (encoderSolve st))
    case res of
        [] -> do
            currSt <- get
            maxDepth <- getExperiment maxApplicationDepth
            when (currSt ^. currentLoc >= maxDepth) (
              do
                mesgChan <- gets $ view messageChan
                liftIO $ writeChan mesgChan (MesgClose CSNoSolution)
                error "cannot find a path")
            modify $ set currentLoc ((currSt ^. currentLoc) + 1)
            st'' <- withTime EncodingTime (incEncoder env st')
            findPath env dst st''
        _  -> return (res, st')

fixEncoder :: MonadIO m 
           => Environment 
           -> RType 
           -> EncodeState 
           -> SplitInfo 
           -> PNSolver m EncodeState
fixEncoder env dst st info = do
    let binds = env ^. boundTypeVars
    cover <- gets (view abstractionCover)
    writeLog 2 "fixEncoder" $ text "new abstraction cover:" <+> pretty (allTypesOf cover)
    (srcTypes, tgt) <- updateSrcTgt env dst
    writeLog 2 "fixEncoder" $ text "fixed parameter types:" <+> pretty srcTypes
    writeLog 2 "fixEncoder" $ text "fixed return type:" <+> pretty tgt
    writeLog 3 "fixEncoder" $ text "get split information" </> pretty info
    modify $ over type2transition (HashMap.filter (not . null))
    (loc, musters, rets, funcs, tid2tr, _) <- prepEncoderArgs env tgt
    liftIO $ execStateT (encoderRefine info musters srcTypes rets funcs tid2tr) st

findProgram :: MonadIO m 
            => Environment -- the search environment
            -> RType       -- the goal type
            -> EncodeState -- intermediate encoding state
            -> PNSolver m RProgram
findProgram env dst st = do
    modify $ set splitTypes Set.empty
    modify $ set typeAssignment Map.empty
    writeLog 2 "findProgram" $ text "calling findProgram"
    (path, st') <- findPath env dst st
    ngm <- gets $ view nameToGroup
    gm <- gets $ view groupMap
    nameMap <- gets $ view nameMapping
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let hoAlias = Map.keysSet $ Map.filter (`elem` hoArgs) nameMap
    paths <- enumeratePath ngm gm hoAlias path
    checkUntilFail st' path paths
  where
    enumeratePath :: MonadIO m 
                  => Map Id Id 
                  -> Map Id (Set Id) 
                  -> Set Id
                  -> [Id]
                  -> PNSolver m [[Id]]
    enumeratePath ngm gm hoArgs path = do
        let getGroup p = fromJust $ Map.lookup p ngm
        let getFuncs p = Map.findWithDefault Set.empty (getGroup p) gm
        let filterFuncs p = let intsct = Set.intersection hoArgs p
                             in if Set.null intsct then p else intsct
        let allPaths = map (Set.toList . filterFuncs . getFuncs) path
        return $ crossProduct allPaths

    generateCode initialFormer src args sigs = do
        tgt <- gets (view targetType)
        cover <- gets (view abstractionCover)
        let bound = env ^. boundTypeVars
        let rets = filter (isSubtypeOf bound tgt) (allTypesOf cover)
        liftIO (evalStateT (generateProgram sigs src args rets) initialFormer)

    removeSuffix = removeLast '|'

    substPair [] = []
    substPair (x:xs) = if pairProj `isPrefixOf` funName x
                          then   ( x { funName = replaceId pairProj "fst" (funName x), funReturn = [head (funReturn x)] } )
                               : ( x { funName = replaceId pairProj "snd" (funName x), funReturn = [funReturn x !! 1] } )
                               : substPair xs
                          else x : substPair xs

    substName [] [] = []
    substName (n:ns) (fc:fcs) = fc { funName = n } : substName ns fcs

    findFunction fm name = fromMaybe (error $ "cannot find function name " ++ name)
                                     (HashMap.lookup name fm)

    fillSketch reps firedTrans = do
        fm <- gets $ view functionMap
        src <- gets $ view sourceTypes
        let args = Map.keys $ foArgsOf env
        writeLog 2 "findPath" $ text "found path" <+> pretty firedTrans
        let sigs = substPair $ substName firedTrans $ map (findFunction fm) reps
        writeLog 2 "findPath" $ text "found filtered sigs" <+> pretty sigs
        let initialFormer = FormerState 0 HashMap.empty [] []
        code <- withTime FormerTime $ generateCode initialFormer src args sigs
        return code

    checkUntilFail st' _ [] = findProgram env dst st'
    checkUntilFail st' reps (path:ps) = do
        writeLog 1 "findPath" $ pretty path
        codeResult <- fillSketch reps path
        checkResult <- withTime TypeCheckTime $ 
                        firstCheckedOrError $ 
                        sortOn length $ Set.toList codeResult
        rs <- getExperiment refineStrategy
        stop <- getExperiment stopRefine
        placeNum <- getExperiment threshold
        cover <- gets $ view abstractionCover
        case checkResult of
            Left code -> do
                mbSln <- checkSolution st' code
                case mbSln of
                    Nothing -> checkUntilFail (st' {prevChecked=True}) reps ps
                    Just p -> do
                        cnt <- getExperiment solutionCnt
                        modify $ set (searchParams . solutionCnt) (cnt-1)
                        if cnt > 1 then checkUntilFail (st' {prevChecked=True}) reps ps
                                   else return p
            Right err 
                | not (doRefine rs) || (stop && coverSize cover >= placeNum) -> 
                    checkUntilFail st' reps ps
                | otherwise -> do
                    cover <- gets $ view abstractionCover
                    funcs <- gets $ view activeSigs
                    modify $ over solverStats (\s -> s {
                    iterations = iterations s + 1
                        , numOfPlaces = Map.insert (iterations s + 1) (coverSize cover) (numOfPlaces s)
                        , numOfTransitions = Map.insert (iterations s + 1) (Set.size funcs) (numOfTransitions s)
                        })
                    nextSolution st' err      

    firstCheckedOrError [] = return (Right (uHole, AScalar (ATypeVarT varName)))
    firstCheckedOrError (x:xs) = do
        res <- parseAndCheck x
        case res of
          Left prog -> return (Left prog)
          Right err -> do
              res' <- firstCheckedOrError xs
              case res' of
                Left prog -> return (Left prog)
                Right _   -> return (Right err)

    pickGeneralization ABottom target = return ABottom
    pickGeneralization ty target = do
        let bound = env ^. boundTypeVars
        ty' <- generalize bound ty
        let unifier = getUnifier bound [(ty', target)]
        guard (isNothing unifier)
        return ty'

    parseAndCheck code = do
        let prog = case parseExp code of
                       ParseOk exp -> toSynquidProgram exp
                       ParseFailed loc err -> error err
        mapping <- gets $ view nameMapping
        writeLog 1 "findProgram" $ text "Find program" <+> pretty (recoverNames mapping prog)
        modify $ set isChecked True
        btm <- bottomUpCheck env prog
        writeLog 3 "findProgram" $ text "bottom up checking get program" <+> pretty (recoverNames mapping btm)
        checkStatus <- gets (view isChecked)
        let tyBtm = typeOf btm
        when checkStatus (solveTypeConstraint env (shape tyBtm) (shape dst))
        tass <- gets (view typeAssignment)
        ifM (gets $ view isChecked)
            (return (Left btm))
            (do
                let tyBtm' = toAbstractType $ stypeSubstitute tass $ shape tyBtm
                let absDst = toAbstractType $ shape dst
                absBtm <- observeT $ pickGeneralization tyBtm' absDst
                return (Right (btm, absBtm)))

    nextSolution st (prog, at) = do
        cover <- gets $ view abstractionCover
        splitInfo <- withTime RefinementTime (refineSemantic env prog at)
        -- add new places and transitions into the petri net
        cover <- gets $ view abstractionCover
        t2tr <- gets $ view type2transition
        modify $ over solverStats (\s -> s {
            numOfPlaces = Map.insert (iterations s) (coverSize cover) (numOfPlaces s)
            , numOfTransitions = Map.insert (iterations s) (transitionNb st) (numOfTransitions s)
        })
        st' <- withTime EncodingTime (fixEncoder env dst st splitInfo)
        findProgram env dst st'

    checkSolution st code = do
        solutions <- gets $ view currentSolutions
        mapping <- gets $ view nameMapping
        let code' = recoverNames mapping code
        disableDemand <- getExperiment disableDemand
        checkedSols <- withTime TypeCheckTime (filterM (liftIO . runGhcChecks disableDemand env dst) [code'])
        if (code' `elem` solutions) || null checkedSols
            then return Nothing
            else do
                msgChan <- gets $ view messageChan
                modify $ over currentSolutions ((:) code')
                let haskellSolution = toHaskellSolution code'
                printSolution haskellSolution
                stats <- gets $ view solverStats
                loc <- gets $ view currentLoc
                let stats' = stats{pathLength = loc}
                liftIO $ writeChan msgChan (MesgP (code', stats'))
                writeLog 1 "findFirstN" $ text (show stats')
                resetTiming
                return $ Just code'

printSolution solution = do
    liftIO $ putStrLn "*******************SOLUTION*********************"
    liftIO $ putStrLn $ "SOLUTION: " ++ solution
    liftIO $ putStrLn "************************************************"

findFirstN :: MonadIO m => Environment -> RType -> EncodeState -> PNSolver m ()
findFirstN env dst st = do
    cnt <- getExperiment solutionCnt
    strategy <- getExperiment refineStrategy
    withTime TotalSearch $ findProgram env dst st
    when (noGarTyGarIdx strategy >= 0) $ do
        resetTiming
        modify $ set (searchParams . refineStrategy) NoGar
        modify $ set (searchParams . stopRefine) False
        modify $ set currentLoc 1
        modify $ set currentSolutions []
        runPNSolver env dst

runPNSolver :: MonadIO m => Environment -> RType -> PNSolver m ()
runPNSolver env t = do
    writeLog 3 "runPNSolver" $ text $ show (allSymbols env)
    initNet env
    st <- withTime TotalSearch $ withTime EncodingTime (resetEncoder env t)
    findFirstN env t st
    msgChan <- gets $ view messageChan
    liftIO $ writeChan msgChan (MesgClose CSNormal)
    return ()

recoverNames :: Map Id Id -> Program t -> Program t
recoverNames mapping (Program (PSymbol sym) t) =
    case Map.lookup sym mapping of
      Nothing -> Program (PSymbol (removeLast '_' sym)) t
      Just name -> Program (PSymbol (removeLast '_' name)) t
recoverNames mapping (Program (PApp fun pArg) t) = Program (PApp fun' pArg') t
  where
    fun' = case Map.lookup fun mapping of
                Nothing -> removeLast '_' fun
                Just name -> removeLast '_' name
    pArg' = map (recoverNames mapping) pArg
recoverNames mapping (Program (PFun x body) t) = Program (PFun x body') t
  where
    body' = recoverNames mapping body

{- helper functions -}

attachLast :: AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
attachLast t (AFunctionT tArg tRes) | isAFunctionT tRes = AFunctionT tArg (attachLast t tRes)
attachLast t (AFunctionT tArg _) = AFunctionT tArg t
attachLast t _ = t

noGarTyGarIdx :: RefineStrategy -> Int
noGarTyGarIdx NoGarTyGar0 = 0
noGarTyGarIdx NoGarTyGarQ = 1
noGarTyGarIdx NoGarTyGar0B = 2
noGarTyGarIdx NoGarTyGarQB = 3
noGarTyGarIdx _ = -1

doRefine :: RefineStrategy -> Bool
doRefine NoGar = False
doRefine NoGar0 = False
doRefine SypetClone = False
doRefine _ = True

updateTy2Tr :: MonadIO m => Id -> AbstractSkeleton -> PNSolver m ()
updateTy2Tr id f = do
    let addTransition k tid = HashMap.insertWith Set.union k (Set.singleton tid)
    let includedTyps = nub (decompose f)
    mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps

updateSrcTgt :: MonadIO m => Environment -> RType -> PNSolver m ([AbstractSkeleton], AbstractSkeleton)
updateSrcTgt env dst = do
    -- reset source and destination types
    let binds = env ^. boundTypeVars
    abstraction <- gets (view abstractionCover)
    tgt <- currentAbst binds abstraction (toAbstractType (shape dst))
    modify $ set targetType tgt

    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    srcTypes <- mapM ( currentAbst binds abstraction
                     . toAbstractType
                     . shape
                     . toMonotype) $ Map.elems foArgs
    modify $ set sourceTypes srcTypes
    return (srcTypes, tgt)

type EncoderArgs = (Int, HashMap Id [Id], [AbstractSkeleton], [FunctionCode], HashMap AbstractSkeleton (Set Id), Bool)

prepEncoderArgs :: MonadIO m => Environment -> AbstractSkeleton -> PNSolver m EncoderArgs
prepEncoderArgs env tgt = do
    cover <- gets $ view abstractionCover
    loc <- gets $ view currentLoc
    funcs <- gets $ view functionMap
    t2tr <- gets $ view type2transition
    musters <- gets $ view mustFirers
    incremental <- getExperiment incrementalSolving
    let bound = env ^. boundTypeVars
    -- let tid2tr = HashMap.foldrWithKey (\k v -> HashMap.insert (show k) v) HashMap.empty t2tr
    let accepts = superTypeOf bound cover tgt
    let rets = sortBy (compareAbstract bound) accepts
    let sigs = HashMap.elems funcs
    let musters' = HashMap.fromList $ removeDups [] $ HashMap.toList musters
    writeLog 3 "prepEncoderArgs" $ text "current must firers" <+> pretty (HashMap.toList musters)
    return (loc, musters', rets, sigs, t2tr, incremental)
  where
    removeDups _ [] = []
    removeDups acc ((x, y):xs) = (x, y \\ acc):(removeDups (acc++y) xs)
    
foArgsOf :: Environment -> Map Id RSchema
foArgsOf = Map.filter (not . isFunctionType . toMonotype) . _arguments

noneInst instMap id t = not (HashMap.member (id, absFunArgs id t) instMap)

diffInst tvs instMap id t = let oldt = snd (fromJust $ HashMap.lookup (id, absFunArgs id t) instMap)
                             in t /= oldt && isSubtypeOf tvs (lastAbstract t) (lastAbstract oldt)

excludeUseless :: MonadIO m => Id -> AbstractSkeleton -> PNSolver m ()
excludeUseless id ty = do
    writeLog 3 "excludeUseless" $ text "delete" <+> pretty id <+> text "::" <+> pretty ty
    instMap <- gets (view instanceMapping)
    let (tid, _) = fromJust (HashMap.lookup (id, absFunArgs id ty) instMap)
    modify $ over toRemove ((:) tid)
    modify $ over funTypes (Set.delete tid)

getGroupRep :: MonadIO m => Id -> PNSolver m [Id]
getGroupRep name = do
    gr <- gets $ view groupRepresentative
    ngm <- gets $ view nameToGroup
    let argGps = maybeToList $ Map.lookup name ngm
    writeLog 3 "getGroupRep" $ text name <+> text "is contained in group" <+> pretty argGps
    let argRp = catMaybes $ map (`Map.lookup` gr) argGps
    return argRp

assemblePair :: AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
assemblePair first secod | absFunArgs "fst" first == absFunArgs "snd" secod =
    let AFunctionT p f = first
        AFunctionT _ s = secod
     in AFunctionT p (AFunctionT f s)
assemblePair first second = error "fst and snd have different arguments"
