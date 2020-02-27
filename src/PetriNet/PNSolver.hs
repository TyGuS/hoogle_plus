{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module PetriNet.PNSolver (runPNSolver) where

import Control.Concurrent.Chan
import Control.Lens
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.Char as Char
import Data.Data (Data)
import Data.Either hiding (fromLeft, fromRight)
import Data.Foldable
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ord
import Data.Tuple
import Debug.Trace
import Language.Haskell.Exts.Parser (ParseResult(..), parseExp)
import Text.Printf
import System.IO

import Database.Convert
import Database.Generate
import Database.Util
import HooglePlus.Abstraction
import HooglePlus.CodeFormer
import HooglePlus.Refinement
import HooglePlus.Stats
import HooglePlus.TypeChecker
import PetriNet.AbstractType
import HooglePlus.GHCChecker
import HooglePlus.FilterTest
import PetriNet.PNEncoder
import PetriNet.Util
import Synquid.Error
import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Util
import Types.Abstract
import Types.Common
import Types.Encoder hiding (incrementalSolving, varName)
import Types.Environment
import Types.Experiments
import Types.IOFormat
import Types.Program
import Types.Solver
import Types.Type
import Types.IOFormat
import Types.TypeChecker

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
    modify $ set (refineState . toRemove) []
    noBlack <- getExperiment disableBlack
    blacks <- liftIO $ readFile "blacklist.txt"
    let sigs' = if noBlack then sigs else Map.withoutKeys sigs (Set.fromList $ words blacks)
    Map.fromList <$> instantiate' sigs'
  where
    instantiate' sigs = do
        tree <- gets $ view (refineState . abstractionCover)
        let typs = allTypesOf tree
        writeLog 2 "instantiate" $ text "Current abstract types:" <+> text (show tree)
        sigs' <- Map.toList <$> mapM freshType sigs
        foldM (\acc -> (<$>) (acc ++) . uncurry (instantiateWith env typs)) [] sigs'

-- add Pair_match function as needed
instantiateWith :: MonadIO m => Environment -> [AbstractSkeleton] -> Id -> RType -> PNSolver m [(Id, AbstractSkeleton)]
-- skip "snd" function, it would be handled together with "fst"
instantiateWith env typs id t | id == "snd" = return []
instantiateWith env typs id t = do
    instMap <- gets $ view (refineState . instanceMapping)
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
    instMap <- gets $ view (refineState . instanceMapping)
    newId <- if pairProj `isPrefixOf` id then freshId (id ++ "_")
                                         else freshId "f"
    -- when same arguments exist for a function, replace it
    unless (noneInst instMap id ty) (excludeUseless id ty)
    modify $ over (typeChecker . nameMapping) (Map.insert newId id)
    modify $ over (statistics . instanceCounts) (HashMap.insertWith (+) id 1 )
    modify $ over (refineState . instanceMapping) (HashMap.insert (id, absFunArgs id ty) (newId, ty))
    writeLog 3 "mkNewSig" $ text id <+> text "==>"  <+> text newId <+> text "::" <+> pretty ty
    return (newId, ty)

splitTransition :: MonadIO m => Environment -> AbstractSkeleton -> Id -> PNSolver m [(Id, AbstractSkeleton)]
splitTransition env newAt fid = do
    rep <- head <$> getGroupRep fid
    sigs <- gets $ view (searchState . currentSigs)
    let ty = lookupWithError "currentSigs" rep sigs
    writeLog 3 "splitTransition" $ text "split transtion" <+> text fid <+> text "::" <+> pretty ty
    allSubsts ty
  where
    allSubsts ty = allSubsts' (lastAbstract ty) (absFunArgs fid ty)

    allSubsts' ret args | pairProj `isPrefixOf` fid = do
        cover <- gets $ view (refineState . abstractionCover)
        nameMap <- gets $ view (typeChecker . nameMapping)
        instMap <- gets $ view (refineState . instanceMapping)
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
        let sigs' = filter (\t -> noneInst instMap pairProj t || diffInst tvs instMap pairProj t) sigs
        mapM (mkNewSig pairProj) sigs'
    allSubsts' ret args = do
        cover <- gets $ view (refineState . abstractionCover)
        nameMap <- gets $ view (typeChecker . nameMapping)
        splits <- gets $ view (refineState . splitTypes)
        instMap <- gets $ view (refineState . instanceMapping)
        let parents = HashMap.keys $ HashMap.filter (Set.member newAt) cover
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
        let sigs' = filter (\t -> noneInst instMap funId t || diffInst tvs instMap funId t) sigs
        mapM (mkNewSig funId) sigs'

    enumArgs parents [] = [[]]
    enumArgs parents (arg:args)
      | arg `elem` parents = let args' = enumArgs parents args
                              in [a:as | a <- [arg, newAt], as <- args']
      | otherwise = map (arg:) (enumArgs parents args)

addSignatures :: MonadIO m => Environment -> PNSolver m (Map Id AbstractSkeleton)
addSignatures env = do
    let foArgs = Map.keys $ foArgsOf env
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    let envSymbols = allSymbols env
    let usefulPipe k _ = k `notElem` foArgs
    let usefulSymbols = Map.filterWithKey usefulPipe envSymbols
    let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let binds = env ^. boundTypeVars
    -- cands <- getExperiment hoCandidates
    envSigs <- instantiate env usefulSymbols
    -- add higher order functions
    -- hoSigs <- mapM (addHoArg env envSigs False) cands
    -- hoSigs' <- mapM (addHoArg env envSigs True) $ Map.keys hoArgs
    -- let sigs = Map.unions $ envSigs : hoSigs ++ hoSigs'
    let sigs = envSigs

    sigs' <- ifM (getExperiment coalesceTypes) (mkGroups env sigs) (return sigs)
    modify $ over (searchState . activeSigs) (Set.union (Map.keysSet sigs'))
    mapM_ addEncodedFunction (Map.toList sigs')
    return sigs'

mkGroups :: MonadIO m => Environment -> Map Id AbstractSkeleton -> PNSolver m (Map Id AbstractSkeleton)
mkGroups env sigs = do
    let encodedSigs = Map.mapWithKey encodeFunction sigs
    (t2g, sigGroups) <- groupSignatures encodedSigs
    -- Do I want to take the sigs here?
    nameMap <- gets $ view (typeChecker . nameMapping)
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let hoAlias = Map.keysSet $ Map.filter (`elem` hoArgs) nameMap
    representatives <- Map.fromList <$> mapM (selectOurRep hoAlias) (Map.toList sigGroups)
    let sigs' = Map.restrictKeys sigs (Set.fromList $ Map.elems representatives)
    modify $ over (groupState . groupRepresentative) $ Map.union representatives
    modify $ over (groupState . typeToGroup) (Map.union t2g)
    mapM_ updateGroups (Map.toList sigGroups)
    return sigs'
  where
    selectOurRep hoAlias (gid, group) = (,) gid <$> selectRepresentative hoAlias gid group
    -- Given the new set of groups, update the current group mapping
    -- updateGroups :: Map Id (Set Id) -> Map Id (Set Id) -> Map Id (Set Id)
    -- updateGroups sigGroups gm = foldr updategm gm (Map.toList sigGroups)
    -- updategm (groupName, groupMembers) = Map.insertWith Set.union groupName groupMembers
    updateGroups (gid, fids) = do
        modify $ over (groupState . groupMap) (Map.insertWith Set.union gid fids)
        mapM_ (\fid -> modify $ over (groupState . nameToGroup) (Map.insert fid gid)) fids

selectRepresentative :: MonadIO m => Set Id -> GroupId -> Set Id -> PNSolver m Id
selectRepresentative hoArgs gid s = do
    let setToPickFrom = s
    strat <- getExperiment coalesceStrategy
    case strat of
        First -> return $ Set.elemAt 0 setToPickFrom
        LeastInstantiated -> pickReprOrder sortOn setToPickFrom
        MostInstantiated -> pickReprOrder sortDesc setToPickFrom
    where
        pickReprOrder sorting setToPickFrom = do
            nm <- gets $ view (typeChecker . nameMapping)
            instCounts <- gets $ view (statistics . instanceCounts)
            let idToCount id = instCounts HashMap.! (nm Map.! id)
            let countMapping = sorting snd $ map (\x -> (x, idToCount x)) $ Set.toList setToPickFrom
            writeLog 3 "SelectRepresentative" $ text gid <+> "needs to pick: " <+> pretty countMapping
            return $ fst $ head countMapping

        sortDesc f = sortBy (on (flip compare) f)


addMusters :: MonadIO m => Id -> PNSolver m ()
addMusters arg = do
    nameMap <- gets $ view (typeChecker . nameMapping)
    let eqArg n = n == arg || n == arg ++ hoPostfix
    let argFuncs = Map.keys $ Map.filter eqArg nameMap
    argRps <- mapM getGroupRep argFuncs
    modify $ over (encoder . refinements . mustFirers) (HashMap.insert arg $ concat argRps)

-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m SplitInfo
refineSemantic env prog at = do
    cover <- gets $ view (refineState . abstractionCover)
    writeLog 2 "instantiate" $ text "Current abstract types:" <+> text (show cover)
    -- back propagation of the error types to get all split information
    propagate env prog $ compactAbstractType at
    -- get the split pairs
    splits <- gets $ view (refineState . splitTypes)
    let tvs = env ^. boundTypeVars
    let sortedSplits = sortBy (flip (compareAbstract tvs)) (Set.toList splits)
    writeLog 3 "refineSemantic splitTypes" $ pretty sortedSplits
    -- get removed transitions
    addsAndRemoves <- mapM splitTransitions sortedSplits
    let (adds, removes) = unzip addsAndRemoves
    let addedSigs = map fst (concat adds)
    let removedSigs = concat removes
    let toAdd = addedSigs \\ removedSigs
    let removables = removedSigs \\ addedSigs
    writeLog 3 "refineSemantic (toAdd, removables)" $ pretty (toAdd, removables)

    -- add clone functions and add them into new transition set
    noClone <- getExperiment disableCopy
    cloneNames <- if noClone then return []
                             else mapM addCloneFunction $ Set.toList splits
    -- update the higer order query arguments
    let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    mapM_ addMusters (Map.keys hoArgs)
    -- call the refined encoder on these signatures and disabled signatures
    return SplitInfo { newPlaces = Set.toList splits
                     , removedTrans = removables
                     , newTrans = addedSigs ++ cloneNames
                     }
  where
    splitTransitions at = do
        modify $ set (refineState . toRemove) []

        cover <- gets $ view (refineState . abstractionCover)
        t2tr <- gets $ view (encoder . variables . type2transition)
        gm <- gets $ view (groupState . groupMap)
        gr <- gets $ view (groupState . groupRepresentative)
        let parents = HashMap.keys $ HashMap.filter (Set.member at) cover
        let pids = Set.unions $ map (\p -> HashMap.lookupDefault Set.empty p t2tr) parents
        let gids = Map.keys $ Map.filter (`Set.member` pids) gr
        let fids = concatMap (\gid -> Set.toList $ Map.findWithDefault Set.empty gid gm) gids
        sigs <- mapM (splitTransition env at) fids
        let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
        let tvs = env ^. boundTypeVars
        let envSigs = Map.fromList (concat sigs)
        let allSigs = concat sigs
        sigs' <- mkGroups env $ Map.fromList allSigs
        mapM_ addEncodedFunction (Map.toList sigs')

        -- update the group information by the current toRemove
        toCoalesce <- getExperiment coalesceTypes
        (toAdd, removables) <- changeGroups toCoalesce
        writeLog 3 "splitTransitions (toAdd, removables)" $ pretty (toAdd, removables)
        mapM_ addEncodedFunction toAdd

        let adds = Map.toList sigs' ++ toAdd
        modify $ over (searchState . activeSigs) (Set.union $ Set.fromList $ map fst adds)
        modify $ over (searchState . activeSigs) (`Set.difference` Set.fromList removables)
        modify $ over (encoder . refinements . mustFirers) (HashMap.map (filter (`notElem` removables)))
        modify $ over (typeChecker . nameMapping) (`Map.withoutKeys` Set.fromList removables)
        return (adds, removables)

    changeGroups True = do
        removables' <- gets $ view (refineState . toRemove)
        writeLog 4 "changeGroups" $ pretty removables'
        splitGroups removables'
    changeGroups False = do
        removables <- gets $ view (refineState . toRemove)
        return ([], removables)

    splitGroups :: MonadIO m => [Id] -> PNSolver m ([(Id, AbstractSkeleton)], [Id])
    splitGroups removables = do
        -- Step 1: modify groupmap to exclude all those in removables
        -- Some groups may no longer exist as a result of this operation.
        -- Some groups representative may not longer be valid.
        -- TODO: This operation could be slow. find a way to go from id -> groupId (or id -> abstrTy)
        gm <- gets $ view (groupState . groupMap)
        let gm' = Map.fromList  $ map (\(a,b) -> (a, fromJust b))
                              $ filter (isJust . snd)
                              $ map (\(k, vs) -> (k, shrinkSet (Set.fromList removables) vs))
                              $ Map.toList gm
        modify $ set (groupState . groupMap) gm'
        grs <- gets $ view (groupState . groupRepresentative)
        -- Step 2: fixup the group representatives, while also deciding what we can safely remove,
        -- and what we need to add (new representatives).
        (toAdd, toRemove, grs') <- foldM updateRepresentatives ([], [], Map.empty) (Map.toList grs)
        modify $ set (groupState . groupRepresentative) grs'
        mapM_ (\t -> modify $ over (encoder . variables . type2transition) (HashMap.map $ Set.delete t)) removables
        let removeSet = Set.fromList removables
        modify $ over (typeChecker . nameMapping) (`Map.withoutKeys` removeSet)
        modify $ over (searchState . currentSigs) (`Map.withoutKeys` removeSet)
        modify $ over (groupState . nameToGroup) (`Map.withoutKeys` removeSet)
        return (toAdd, toRemove)

    updateRepresentatives :: MonadIO m 
                          => ([(Id, AbstractSkeleton)], [Id], Map GroupId Id) 
                          -> (GroupId, Id) 
                          -> PNSolver m ([(Id, AbstractSkeleton)], [Id], Map GroupId Id)
    updateRepresentatives (addables, removables, newReps) (gid, rep)= do
        gm <- gets $ view (groupState . groupMap)
        sigs <- gets $ view (searchState . currentSigs)
        nameMap <- gets $ view (typeChecker . nameMapping)
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
                        newRep <- selectRepresentative hoAlias gid currentGroup
                        let abstractType = lookupWithError "currentSigs" rep sigs
                        let addables' = (newRep, abstractType):addables
                        return (addables', removables', Map.insert gid newRep newReps)
                      -- There was only that one element in the group, so with the leader gone, the group goes away.
                      else return (addables, removables', newReps)
                -- after all the removals, the current representative is still in its original group
                else return (addables, removables, Map.insert gid rep newReps)

    shrinkSet :: Set Id -> Set Id -> Maybe (Set Id)
    shrinkSet toRemove ids = let
        ids' = Set.difference ids toRemove
        in if Set.null ids' then Nothing else Just ids'


initNet :: MonadIO m => Environment -> PNSolver m ()
initNet env = withTime ConstructionTime $ do
    -- reset the solver state
    modify $ set (searchState . functionMap) HashMap.empty
    modify $ set (searchState . currentSigs) Map.empty
    modify $ set (encoder . variables . type2transition) HashMap.empty
    modify $ set (refineState . instanceMapping) HashMap.empty

    addSignatures env
    -- add clone functions for each type
    noClone <- getExperiment disableCopy
    unless noClone $ do
        ty2tr <- gets $ view (encoder . variables . type2transition)
        let allTy = HashMap.keys ty2tr
        mapM_ addCloneFunction allTy
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
    modify $ over (searchState . functionMap) (HashMap.insert id ef)
    modify $ over (searchState . currentSigs) (Map.insert id f)
    -- store the used abstract types and their groups into mapping
    updateTy2Tr id f

resetEncoder :: (MonadIO m) => Environment -> RType -> PNSolver m ()
resetEncoder env dst = do
    (srcTypes, tgt) <- updateSrcTgt env dst
    writeLog 2 "resetEncoder" $ text "parameter types are" <+> pretty srcTypes
    writeLog 2 "resetEncoder" $ text "return type is" <+> pretty tgt
    encodeState <- gets $ view encoder
    params <- gets $ view searchParams
    (loc, rets, funcs) <- prepEncoderArgs env tgt
    let encoder' = encodeState { _encSearchParams = params }
    st <- liftIO $ encoderInit encoder' loc srcTypes rets funcs 
    modify $ set encoder st

incEncoder :: MonadIO m => Environment -> PNSolver m ()
incEncoder env = do
    tgt <- gets $ view (refineState . targetType)
    src <- gets $ view (refineState . sourceTypes)
    (_, rets, funcs) <- prepEncoderArgs env tgt
    st <- gets $ view encoder
    st' <- liftIO $ execStateT (encoderInc funcs src rets) st
    modify $ set encoder st'

findPath :: MonadIO m
         => Environment
         -> RType
         -> PNSolver m [Id]
findPath env dst = do
    st <- gets $ view encoder
    (res, st') <- withTime SolverTime $ liftIO $ encoderSolve st
    modify $ set encoder st'
    case res of
        [] -> do
            loc <- gets $ view (searchState . currentLoc)
            maxDepth <- getExperiment maxApplicationDepth
            when (loc >= maxDepth) $ do
                mesgChan <- gets $ view messageChan
                liftIO $ writeChan mesgChan (MesgClose CSNoSolution)
                error "cannot find a path"
            modify $ set (searchState . currentLoc) (loc + 1)
            withTime EncodingTime $ incEncoder env
            findPath env dst
        _  -> return res

fixEncoder :: MonadIO m
           => Environment
           -> RType
           -> SplitInfo
           -> PNSolver m ()
fixEncoder env dst info = do
    st <- gets $ view encoder
    cover <- gets $ view (refineState . abstractionCover)
    writeLog 2 "fixEncoder" $ text "new abstraction cover:" <+> pretty (allTypesOf cover)
    (srcTypes, tgt) <- updateSrcTgt env dst
    writeLog 2 "fixEncoder" $ text "fixed parameter types:" <+> pretty srcTypes
    writeLog 2 "fixEncoder" $ text "fixed return type:" <+> pretty tgt
    writeLog 3 "fixEncoder" $ text "get split information" </> pretty info
    modify $ over (encoder . variables . type2transition) (HashMap.filter (not . null))
    (loc, rets, _) <- prepEncoderArgs env tgt
    fm <- gets $ view (searchState . functionMap)
    let funcs = map (fromJust . (`HashMap.lookup` fm)) (newTrans info)
    st' <- liftIO $ execStateT (encoderRefine info srcTypes rets funcs) st
    modify $ set encoder st'

findProgram :: MonadIO m
            => Environment -- the search environment
            -> RType       -- the goal type
            -> [Example]   -- examples for post-filtering
            -> Int         -- remaining number of solutions to be found
            -> PNSolver m ()
findProgram env goal examples cnt = do
    let dst = lastType goal
    modify $ set (refineState . splitTypes) Set.empty
    modify $ set (refineState . passOneOrMore) False
    modify $ set (typeChecker . typeAssignment) Map.empty
    writeLog 2 "findProgram" $ text "calling findProgram"
    path <- findPath env dst
    writeLog 2 "findProgram" $ text "unfiltered path:" <+> pretty path
    let usefulTrans = filter skipClone path
    searchResults <- withTime FormerTime $ observeManyT cnt $
        enumeratePath env goal examples usefulTrans
    mapM_ handleResult searchResults
    let solnNum = length searchResults
    when (solnNum < cnt) -- get enough solutions, search search
         (nextSolution env goal examples (cnt - solnNum))
    where
        handleResult NotFound = error "NotFound appeared in search results"
        handleResult (Found out@(Output soln exs)) = do
            writeSolution out
            modify $ over (searchState . currentSolutions) ((:) soln)
        handleResult (MoreRefine err)  = error "Should not encounter more refine"

        skipClone = not . isInfixOf "|clone"

enumeratePath :: MonadIO m 
              => Environment
              -> RType
              -> [Example] 
              -> [Id] 
              -> BackTrack m SearchResult
enumeratePath env goal examples path = do
    ngm <- gets $ view (groupState . nameToGroup)
    gm <- gets $ view (groupState . groupMap)
    uc <- gets $ view (statistics . useCount)
    nameMap <- gets $ view (typeChecker . nameMapping)
    disrel <- getExperiment disableRelevancy
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let getGroup p = lookupWithError "nameToGroup" p ngm
    let getFuncs p = Map.findWithDefault Set.empty (getGroup p) gm
    let substName x = lookupWithError "nameMapping" x nameMap
    let nameCount x = Map.findWithDefault 0 (substName x) uc
    let sortFuncs p = sortOn nameCount $ Set.toList p
    let allPaths = map (sortFuncs . getFuncs) path
    let getRealName x = replaceId hoPostfix "" $ lookupWithError "nameMapping" x nameMap
    let filterPaths p = disrel || all (`elem` map getRealName p) hoArgs
    -- ensure the usage of all the higher order arguments
    guard (filterPaths path)
    msum $ map (checkPath env goal examples) (sequence allPaths)

checkPath :: MonadIO m 
          => Environment 
          -> RType
          -> [Example] 
          -> [Id] 
          -> BackTrack m SearchResult
checkPath env goal examples path = do
    -- fill the sketch with the functions in the path
    codeResult <- fillSketch env path
    writeLog 1 "checkPath" $ pretty codeResult
    let dst = lastType goal
    checkResult <- withTime TypeCheckTime $ parseAndCheck env dst codeResult
    writeLog 1 "checkPath" $ text "get result" <+> text (show checkResult)
    rs <- getExperiment refineStrategy
    stop <- getExperiment stopRefine
    placeNum <- getExperiment threshold
    cover <- gets $ view (refineState . abstractionCover)
    case checkResult of
        Left code -> writeStats >> checkSolution env goal examples code
        Right err -> do
            modify $ set (refineState . lastError) err
            let stopRefine = not (doRefine rs) || (stop && coverSize cover >= placeNum)
            when stopRefine $ modify $ set (refineState . passOneOrMore) True
            mzero
    where
        writeStats = do
            cover <- gets $ view (refineState . abstractionCover)
            fm <- gets $ view (searchState . functionMap)
            currIter <- gets $ view (statistics . solverStats . iterations)
            let nextIter = currIter + 1
            modify $ over (statistics . solverStats . numOfPlaces) (Map.insert nextIter (coverSize cover))
            modify $ over (statistics . solverStats . numOfTransitions) (Map.insert nextIter (HashMap.size fm))

-- TODO: maybe we can change the order here
-- once we get a correct solution, stop the refine
parseAndCheck :: MonadIO m => Environment -> RType -> String -> BackTrack m (Either RProgram CheckError)
parseAndCheck env dst code = do
    prog <- case parseExp code of
                ParseOk exp         -> return (toSynquidProgram exp)
                ParseFailed loc err -> mzero
    mapping <- gets $ view (typeChecker . nameMapping)
    counter <- gets $ view (typeChecker . nameCounter)
    writeLog 1 "parseAndCheck" $ text "Find program" <+> pretty (recoverNames mapping prog)
    checkerState <- gets $ view typeChecker
    let checkerState' = checkerState { _isChecked = True }
    (btm, checkerState) <- runStateT (bottomUpCheck env prog) checkerState'
    modify $ set typeChecker checkerState
    writeLog 2 "parseAndCheck" $ text "bottom up checking get program" <+> pretty (recoverNames mapping btm)
    let checkStatus = checkerState ^. isChecked
    let tyBtm = typeOf btm
    writeLog 2 "parseAndCheck" $ pretty tyBtm <+> pretty dst
    checkerState' <- if checkStatus then execStateT (solveTypeConstraint env (shape tyBtm) (shape dst)) checkerState
                                    else return checkerState
    let tass = checkerState' ^. typeAssignment
    modify $ set typeChecker checkerState'
    if checkerState' ^. isChecked
        then do
            modify $ set (refineState . passOneOrMore) True
            return (Left btm)
        else do
            writeLog 1 "parseAndCheck" $ text "Generalizing abstract type" <+> pretty tyBtm
            let tyBtm' = toAbstractType $ stypeSubstitute tass $ shape tyBtm
            let absDst = toAbstractType $ shape dst
            absBtm <- observeT $ pickGeneralization env tyBtm' absDst
            return (Right (btm, absBtm))

pickGeneralization :: MonadIO m 
                   => Environment 
                   -> AbstractSkeleton 
                   -> AbstractSkeleton 
                   -> LogicT (BackTrack m) AbstractSkeleton
pickGeneralization _ ABottom _ = return ABottom
pickGeneralization env ty target = do
    let bound = env ^. boundTypeVars
    ty' <- lift $ generalize bound ty
    let unifier = getUnifier bound [(ty', target)]
    guard (isNothing unifier)
    return ty'

fillSketch :: MonadIO m => Environment -> [Id] -> BackTrack m String
fillSketch env firedTrans = do
    src <- gets $ view (refineState . sourceTypes)
    nameMap <- gets $ view (typeChecker . nameMapping)
    repLists <- lift $ mapM getGroupRep firedTrans
    fm <- gets $ view (searchState . functionMap)
    let args = Map.keys $ foArgsOf env
    writeLog 1 "fillSketch" $ text "found path" <+> pretty firedTrans
    mapM_ (\f -> do
         let name = lookupWithError "nameMapping" f nameMap
         modify $ over (statistics . useCount) $ Map.insertWith (+) name 1) firedTrans
    let reps = map head repLists
    let sigs = substPair $ substName firedTrans $ map (findFunction fm) reps
    writeLog 1 "fillSketch" $ text "found filtered sigs" <+> pretty sigs
    let initialFormer = FormerState HashMap.empty []
    progSet <- withTime FormerTime $ generateCode initialFormer env src args sigs
    let progList = sortOn (Data.Ord.Down . length) $ Set.toList progSet
    msum $ map return progList
    where
        substPair [] = []
        substPair (x:xs) 
            | pairProj `isPrefixOf` funName x =
                ( x { funName = replaceId pairProj "fst" (funName x), funReturn = [head (funReturn x)] } )
              : ( x { funName = replaceId pairProj "snd" (funName x), funReturn = [funReturn x !! 1] } )
              : substPair xs
            | otherwise = x : substPair xs

generateCode :: MonadIO m
             => FormerState
             -> Environment
             -> [AbstractSkeleton]
             -> [Id]
             -> [FunctionCode]
             -> BackTrack m (Set String)
generateCode initialFormer env src args sigs = do
    tgt <- gets $ view (refineState . targetType)
    cover <- gets $ view (refineState . abstractionCover)
    disrel <- getExperiment disableRelevancy
    let bound = env ^. boundTypeVars
    let rets = filter (isSubtypeOf bound tgt) (allTypesOf cover)
    writeLog 1 "generateCode" $ pretty src
    writeLog 1 "generateCode" $ pretty rets
    liftIO (evalStateT (generateProgram sigs src args rets disrel) initialFormer)

nextSolution :: MonadIO m 
             => Environment 
             -> RType 
             -> [Example] 
             -> Int
             -> PNSolver m ()
nextSolution env goal examples cnt = do
    hasPass <- gets $ view (refineState . passOneOrMore)
    if hasPass -- block the previous path and then search
       then blockCurrent >> findProgram env goal examples cnt
       else do -- refine and then search
            let dst = lastType goal
            cover <- gets $ view (refineState . abstractionCover)
            (prog, at) <- gets $ view (refineState . lastError)
            splitInfo <- withTime RefinementTime (refineSemantic env prog at)
            writeLog 1 "nextSolution" $ text "get split info" <+> pretty splitInfo
            -- add new places and transitions into the petri net
            cover <- gets $ view (refineState . abstractionCover)
            funcs <- gets $ view (searchState . functionMap)
            currIter <- gets $ view (statistics . solverStats . iterations)
            modify $ over (statistics . solverStats . iterations) (+ 1)
            modify $ over (statistics . solverStats . numOfPlaces)
                          (Map.insert currIter (coverSize cover))
            modify $ over (statistics . solverStats . numOfTransitions)
                          (Map.insert currIter (HashMap.size funcs))
            withTime EncodingTime $ fixEncoder env dst splitInfo
            findProgram env goal examples cnt
    where
        blockCurrent = modify $ set (encoder . increments . prevChecked) True

checkSolution :: MonadIO m 
              => Environment 
              -> RType 
              -> [Example] 
              -> RProgram 
              -> BackTrack m SearchResult
checkSolution env goal examples code = do
    solutions <- gets $ view (searchState . currentSolutions)
    mapping <- gets $ view (typeChecker . nameMapping)
    params <- gets $ view searchParams
    msgChan <- gets $ view messageChan
    let code' = recoverNames mapping code
    checkResult <- withTime TypeCheckTime $ 
        liftIO $ check env params examples code' goal msgChan
    if (code' `elem` solutions) || isNothing checkResult
        then mzero
        else return $ Found $ Output code' (fromJust checkResult)

runPNSolver :: MonadIO m => Environment -> RType -> [Example] -> PNSolver m ()
runPNSolver env goal examples = do
    writeLog 3 "runPNSolver" $ text $ show (allSymbols env)
    cnt <- getExperiment solutionCnt
    withTime TotalSearch $ initNet env
    let t = lastType goal
    withTime TotalSearch $ withTime EncodingTime $ resetEncoder env t
    -- findFirstN env goal st examples [] cnt
    findProgram env goal examples cnt
    msgChan <- gets $ view messageChan
    liftIO $ writeChan msgChan (MesgClose CSNormal)

{- helper functions -}
writeSolution :: MonadIO m => Output -> PNSolver m ()
writeSolution out = do
    stats <- gets $ view (statistics . solverStats)
    loc <- gets $ view (searchState . currentLoc)
    msgChan <- gets $ view messageChan
    let stats' = stats { _pathLength = loc }
    liftIO $ writeChan msgChan (MesgP (out, stats', undefined))
    -- liftIO $ printSolution code
    -- liftIO $ hFlush stdout
    writeLog 1 "writeSolution" $ text (show stats')

recoverNames :: Map Id Id -> Program t -> Program t
recoverNames mapping (Program (PSymbol sym) t) =
    case Map.lookup sym mapping of
      Nothing -> Program (PSymbol (replaceId hoPostfix "" $ removeLast '_' sym)) t
      Just name -> Program (PSymbol (replaceId hoPostfix "" $ removeLast '_' name)) t
recoverNames mapping (Program (PApp fun pArg) t) = Program (PApp fun' pArg') t
  where
    fun' = case Map.lookup fun mapping of
                Nothing -> replaceId hoPostfix "" $ removeLast '_' fun
                Just name -> replaceId hoPostfix "" $ removeLast '_' name
    pArg' = map (recoverNames mapping) pArg
recoverNames mapping (Program (PFun x body) t) = Program (PFun x body') t
  where
    body' = recoverNames mapping body

substName :: [Id] -> [FunctionCode] -> [FunctionCode]
substName [] [] = []
substName (n:ns) (fc:fcs) = fc { funName = n } : substName ns fcs

addCloneFunction :: MonadIO m => AbstractSkeleton -> PNSolver m Id
addCloneFunction ty = do
    let fname = show ty ++ "|clone"
    let fc = FunctionCode fname [] [ty] [ty, ty]
    modify $ over (searchState . functionMap) (HashMap.insert fname fc)
    updateTy2Tr fname ty
    return fname

doRefine :: RefineStrategy -> Bool
doRefine NoGar = False
doRefine NoGar0 = False
doRefine SypetClone = False
doRefine _ = True

updateTy2Tr :: MonadIO m => Id -> AbstractSkeleton -> PNSolver m ()
updateTy2Tr id f = do
    let addTransition k tid = HashMap.insertWith Set.union k (Set.singleton tid)
    let includedTyps = nub (decompose f)
    mapM_ (\t -> modify $ over (encoder . variables . type2transition) (addTransition t id)) includedTyps

updateSrcTgt :: MonadIO m
            => Environment
            -> RType
            -> PNSolver m ([AbstractSkeleton], AbstractSkeleton)
updateSrcTgt env dst = do
    -- reset source and destination types
    let binds = env ^. boundTypeVars
    abstraction <- gets $ view (refineState . abstractionCover)
    tgt <- currentAbst binds abstraction (toAbstractType (shape dst))
    modify $ set (refineState . targetType) tgt

    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    srcTypes <- mapM ( currentAbst binds abstraction
                     . toAbstractType
                     . shape
                     . toMonotype) $ Map.elems foArgs
    modify $ set (refineState . sourceTypes) srcTypes
    return (srcTypes, tgt)

type EncoderArgs = (Int, [AbstractSkeleton], [FunctionCode])

prepEncoderArgs :: MonadIO m
                => Environment
                -> AbstractSkeleton
                -> PNSolver m EncoderArgs
prepEncoderArgs env tgt = do
    cover <- gets $ view (refineState . abstractionCover)
    loc <- gets $ view (searchState . currentLoc)
    funcs <- gets $ view (searchState . functionMap)
    let bound = env ^. boundTypeVars
    let accepts = superTypeOf bound cover tgt
    let rets = sortBy (compareAbstract bound) accepts
    let sigs = HashMap.elems funcs
    return (loc, rets, sigs)

foArgsOf :: Environment -> Map Id RSchema
foArgsOf = Map.filter (not . isFunctionType . toMonotype) . _arguments

noneInst instMap id t = not (HashMap.member (id, absFunArgs id t) instMap)

diffInst tvs instMap id t = let oldt = snd (fromJust $ HashMap.lookup (id, absFunArgs id t) instMap)
                             in t /= oldt && isSubtypeOf tvs (lastAbstract t) (lastAbstract oldt)

excludeUseless :: MonadIO m => Id -> AbstractSkeleton -> PNSolver m ()
excludeUseless id ty = do
    instMap <- gets $ view (refineState . instanceMapping)
    let (tid, ty') = fromJust (HashMap.lookup (id, absFunArgs id ty) instMap)
    writeLog 3 "excludeUseless" $ text "delete" <+> pretty tid <+> text "==>" <+> pretty id <+> text "::" <+> pretty ty'
    modify $ over (refineState . toRemove) ((:) tid)

getGroupRep :: MonadIO m => Id -> PNSolver m [Id]
getGroupRep name = do
    gr <- gets $ view (groupState . groupRepresentative)
    ngm <- gets $ view (groupState . nameToGroup)
    let argGps = maybeToList $ Map.lookup name ngm
    writeLog 3 "getGroupRep" $ text name <+> text "is contained in group" <+> pretty argGps
    let argRp = mapMaybe (`Map.lookup` gr) argGps
    if null argRp then error ("cannot find group rep for " ++ name) else return argRp

assemblePair :: AbstractSkeleton
            -> AbstractSkeleton
            -> AbstractSkeleton
assemblePair first secod | absFunArgs "fst" first == absFunArgs "snd" secod =
    let AFunctionT p f = first
        AFunctionT _ s = secod
     in AFunctionT p (AFunctionT f s)
assemblePair first second = error "fst and snd have different arguments"

findFunction :: HashMap Id FunctionCode -> Id -> FunctionCode
findFunction fm name = fromMaybe (error $ "cannot find function name " ++ name)
                                 (HashMap.lookup name fm)
