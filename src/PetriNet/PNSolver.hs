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
import System.Console.ANSI

import Database.Convert
import Database.Generate
import Database.Util
import HooglePlus.Abstraction
import HooglePlus.CodeFormer
import HooglePlus.Refinement
import HooglePlus.Stats
import HooglePlus.TypeChecker
import HooglePlus.BiExplorer
import PetriNet.AbstractType
import PetriNet.GHCChecker
import PetriNet.PNEncoder
import PetriNet.Util
import Synquid.Error
import Synquid.Logic hiding (varName)
import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Util
import Types.Abstract
import Types.Common
import Types.Encoder hiding (incrementalSolving, mustFirers, varName)
import Types.Environment
import Types.Experiments
import Types.Program
import Types.Solver
import Types.Type
import Types.Checker

selection = True

instantiate :: MonadIO m => Environment -> Map Id RSchema -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs = do
    modify $ set toRemove []
    noBlack <- getExperiment disableBlack
    blacks <- liftIO $ readFile "config/blacklist.txt"
    let sigs' = if noBlack then sigs else Map.withoutKeys sigs (Set.fromList $ words blacks)
    Map.fromList <$> instantiate' sigs'
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
            nameMap <- gets $ view nameMapping
            first <- freshType (Monotype t)
            secod <- freshType $ findSymbol nameMap env "snd"
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
    modify $ over instanceCounts (HashMap.insertWith (+) id 1 )
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
    allSubsts ty = allSubsts' (lastAbstract ty) (absFunArgs fid ty)

    allSubsts' ret args | pairProj `isPrefixOf` fid = do
        cover <- gets $ view abstractionCover
        nameMap <- gets $ view nameMapping
        instMap <- gets $ view instanceMapping
        let parents = HashMap.keys $ HashMap.filter (Set.member newAt) cover
        let args' = enumArgs parents (take 1 args)
        let fstRet = args !! 1
        let sndRet = ret
        fstType <- freshType $ findSymbol nameMap env "fst"
        sndType <- freshType $ findSymbol nameMap env "snd"
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
        cover <- gets $ view abstractionCover
        nameMap <- gets $ view nameMapping
        splits <- gets $ view splitTypes
        instMap <- gets $ view instanceMapping
        let parents = HashMap.keys $ HashMap.filter (Set.member newAt) cover
        let args' = enumArgs parents args
        writeLog 3 "allSubsts'" $ pretty args'
        funType <- freshType $ findSymbol nameMap env fid
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

addSignatures :: MonadIO m => Environment -> Map Id RSchema -> PNSolver m (Map Id AbstractSkeleton)
addSignatures env usefulSymbols = do
    sigs <- instantiate env usefulSymbols
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
    let hoArgs = Map.keys $ Map.filter isFunctionType (env ^. arguments)
    let hoAlias = Map.keysSet $ Map.filter (`elem` hoArgs) nameMap
    representatives <- Map.fromList <$> mapM (selectOurRep hoAlias) (Map.toList sigGroups)
    let sigs' = Map.restrictKeys sigs (Set.fromList $ Map.elems representatives)
    modify $ over groupRepresentative $ Map.union representatives
    modify $ over typeToGroup (Map.union t2g)
    mapM_ updateGroups (Map.toList sigGroups)
    return sigs'
  where
    selectOurRep hoAlias (gid, group) = (,) gid <$> selectRepresentative hoAlias gid group
    -- Given the new set of groups, update the current group mapping
    -- updateGroups :: Map Id (Set Id) -> Map Id (Set Id) -> Map Id (Set Id)
    -- updateGroups sigGroups gm = foldr updategm gm (Map.toList sigGroups)
    -- updategm (groupName, groupMembers) = Map.insertWith Set.union groupName groupMembers
    updateGroups (gid, fids) = do
        modify $ over groupMap (Map.insertWith Set.union gid fids)
        mapM_ (\fid -> modify $ over nameToGroup (Map.insert fid gid)) fids

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
            nm <- gets $ view nameMapping
            instCounts <- gets $ view instanceCounts
            let idToCount id = instCounts HashMap.! (nm Map.! id)
            let countMapping = sorting snd $ map (\x -> (x, idToCount x)) $ Set.toList setToPickFrom
            writeLog 3 "SelectRepresentative" $ text gid <+> "needs to pick: " <+> pretty countMapping
            return $ fst $ head countMapping

        sortDesc f = sortBy (on (flip compare) f)


addMusters :: MonadIO m => Id -> PNSolver m ()
addMusters arg = do
    nameMap <- gets $ view nameMapping
    let eqArg n = n == arg || n == arg ++ hoPostfix
    let argFuncs = Map.keys $ Map.filter eqArg nameMap
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
    let sortedSplits = sortBy (flip (compareAbstract tvs)) (Set.toList splits)
    writeLog 2 "refineSemantic splitTypes" $ pretty sortedSplits
    -- get removed transitions
    addsAndRemoves <- mapM splitTransitions sortedSplits
    let (adds, removes) = unzip addsAndRemoves
    let addedSigs = map fst (concat adds)
    let removedSigs = concat removes
    let toAdd = addedSigs \\ removedSigs
    let removables = removedSigs \\ addedSigs
    writeLog 2 "refineSemantic (toAdd, removables)" $ pretty (toAdd, removables)

    -- add clone functions and add them into new transition set
    noClone <- getExperiment disableCopy
    cloneNames <- if noClone then return []
                             else mapM addCloneFunction $ Set.toList splits
    -- update the higer order query arguments
    let hoArgs = Map.filter isFunctionType (env ^. arguments)
    mapM_ addMusters (Map.keys hoArgs)
    -- call the refined encoder on these signatures and disabled signatures
    return SplitInfo { newPlaces = Set.toList splits
                     , removedTrans = removables
                     , newTrans = addedSigs ++ cloneNames
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
        let hoArgs = Map.filter isFunctionType (env ^. arguments)
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
        modify $ over activeSigs (Set.union $ Set.fromList $ map fst adds)
        modify $ over activeSigs (`Set.difference` Set.fromList removables)
        modify $ over mustFirers (HashMap.map (filter (`notElem` removables)))
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
                        let hoArgs = Map.keys $ Map.filter isFunctionType (env ^. arguments)
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
    modify $ set functionMap HashMap.empty
    modify $ set currentSigs Map.empty
    modify $ set type2transition HashMap.empty
    modify $ set instanceMapping HashMap.empty

    names <- if selection then selectComp env >> gets (view (explorer . selectedNames))
                          else return $ Map.keysSet (env ^. symbols)
    let mySymbols = Map.restrictKeys (env ^. symbols) names
    addSignatures env mySymbols
    -- add clone functions for each type
    noClone <- getExperiment disableCopy
    unless noClone $ do
        allTy <- gets (HashMap.keys . view type2transition)
        mapM_ addCloneFunction allTy
    -- add higher order query arguments
    let hoArgs = Map.filter isFunctionType (env ^. arguments)
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
    incremental <- getExperiment incrementalSolving
    relevancy <- getExperiment disableRelevancy
    noClone <- getExperiment disableCopy
    (loc, musters, rets, funcs, tid2tr) <- prepEncoderArgs env tgt
    liftIO $ encoderInit loc musters srcTypes rets funcs tid2tr incremental relevancy noClone

incEncoder :: MonadIO m => Environment -> EncodeState -> PNSolver m EncodeState
incEncoder env st = do
    tgt <- gets (view targetType)
    src <- gets (view sourceTypes)
    (_, _, rets, funcs, _) <- prepEncoderArgs env tgt
    liftIO $ execStateT (encoderInc funcs src rets) st

findPath :: MonadIO m
         => Environment
         -> RType
         -> EncodeState
         -> PNSolver m ([Id], EncodeState)
findPath env dst st = do
    (res, st') <- withTime SolverTime (liftIO (encoderSolve st))
    currSt <- get
    maxDepth <- getExperiment maxApplicationDepth
    currMaxDepth <- gets (view (explorer . currentDepth))
    writeLog 0 "findPath" $ text "current depth" <+> pretty (currSt ^. currentLoc)
    case res of
        [] | currSt ^. currentLoc >= maxDepth -> do
                mesgChan <- gets $ view messageChan
                liftIO $ writeChan mesgChan (MesgClose CSNoSolution)
                error "cannot find a path"
            | currSt ^. currentLoc >= currMaxDepth && selection -> do
                addOrNot <- addMoreComponent
                case addOrNot of
                    Right newNames -> do
                        writeLog 1 "findPath" $ text "adding functions:" <+> pretty newNames
                        st'' <- fixEncoder env dst st' (SplitInfo [] [] newNames)
                        findPath env dst st''
                    Left _ -> do
                        modify $ set (explorer . currentDepth) maxDepth
                        modify $ set currentLoc ((currSt ^. currentLoc) + 1)
                        st'' <- withTime EncodingTime (incEncoder env st')
                        findPath env dst st''
            | otherwise -> do
                modify $ set currentLoc ((currSt ^. currentLoc) + 1)
                st'' <- withTime EncodingTime (incEncoder env st')
                findPath env dst st''
        _  -> return (res, st')
    where
        addMoreComponent = do
            oldNames <- gets (view (explorer . selectedNames))
            writeLog 1 "addMoreComponents" $ pretty oldNames
            selectComp env
            names <- gets (view (explorer . selectedNames))
            if oldNames == names
                then do
                    maxDepth <- getExperiment maxApplicationDepth
                    currDepth <- gets (view currentLoc)
                    if currDepth >= maxDepth
                        then do
                            mesgChan <- gets $ view messageChan
                            liftIO $ writeChan mesgChan (MesgClose CSNoSolution)
                            error "cannot find a path"
                        else return (Left names)
                else do
                    let newNames = names `Set.difference` oldNames
                    let mySymbols = Map.restrictKeys (env ^. symbols) newNames
                    sigs <- addSignatures env mySymbols
                    let hoArgs = Map.filter isFunctionType (env ^. arguments)
                    mapM_ addMusters (Map.keys hoArgs)
                    return $ Right (Map.keys sigs)

fixEncoder :: MonadIO m
           => Environment
           -> RType
           -> EncodeState
           -> SplitInfo
           -> PNSolver m EncodeState
fixEncoder env dst st info = do
    cover <- gets (view abstractionCover)
    writeLog 2 "fixEncoder" $ text "new abstraction cover:" <+> pretty (allTypesOf cover)
    (srcTypes, tgt) <- updateSrcTgt env dst
    writeLog 2 "fixEncoder" $ text "fixed parameter types:" <+> pretty srcTypes
    writeLog 2 "fixEncoder" $ text "fixed return type:" <+> pretty tgt
    writeLog 3 "fixEncoder" $ text "get split information" </> pretty info
    modify $ over type2transition (HashMap.filter (not . null))
    (loc, musters, rets, _, tid2tr) <- prepEncoderArgs env tgt
    fm <- gets $ view functionMap
    -- writeLog 1 "fixEncoder" $ text "test before fromJust"
    -- writeLog 1 "fixEncoder" $ pretty fm
    let funcs = map (fromJust . (`HashMap.lookup` fm)) (newTrans info)
    liftIO $ execStateT (encoderRefine info musters srcTypes rets funcs tid2tr) st

findProgram :: MonadIO m
            => Environment -- the search environment
            -> RType       -- the goal type
            -> EncodeState -- intermediate encoding state
            -> [[Id]]      -- remaining paths
            -> PNSolver m (RProgram, EncodeState, [[Id]])
findProgram env dst st ps
    | not (null ps) = checkUntilFail st ps
    | null ps = do
        modify $ set splitTypes Set.empty
        -- types <- gets (view (explorer . selectedTypes))
        -- names <- gets (view (explorer . selectedNames))
        -- writeLog 1 "selectComp" $ pretty types
        -- writeLog 1 "selectComp" $ pretty names
        writeLog 2 "findProgram" $ text "calling findProgram"
        (path, st') <- findPath env dst st
        writeLog 2 "findProgram" $ text "unfiltered path:" <+> pretty path
        let usefulTrans = filter skipClone path
        paths <- enumeratePath usefulTrans
        writeLog 2 "findProgram" $ text "all possible paths" <+> pretty paths
        checkUntilFail st' paths
  where
    enumeratePath :: MonadIO m => [Id] -> PNSolver m [[Id]]
    enumeratePath path = do
        ngm <- gets $ view nameToGroup
        gm <- gets $ view groupMap
        uc <- gets $ view useCount
        nameMap <- gets $ view nameMapping
        disrel <- getExperiment disableRelevancy
        let hoArgs =
                Map.keys $
                Map.filter isFunctionType (env ^. arguments)
        let hoArgs' = map (++ hoPostfix) hoArgs ++ hoArgs
        let hoAlias = Map.keysSet $ Map.filter (`elem` hoArgs') nameMap
        let getGroup p = lookupWithError "nameToGroup" p ngm
        let getFuncs p = Map.findWithDefault Set.empty (getGroup p) gm
        let sortFuncs p =
                sortOn
                    (\x ->
                         let name = lookupWithError "nameMapping" x nameMap
                          in Map.findWithDefault 0 name uc) $
                Set.toList p
        let allPaths = map (sortFuncs . getFuncs) path
        let filterPaths p =
                let p' =
                        map
                            (\x ->
                                 replaceId hoPostfix "" $
                                 lookupWithError "nameMapping" x nameMap)
                            p
                 in disrel || all (`elem` p') hoArgs
        return $ filter filterPaths (sequence allPaths)

    skipClone = not . isInfixOf "|clone"

    generateCode initialFormer src args sigs = do
        tgt <- gets (view targetType)
        cover <- gets (view abstractionCover)
        disrel <- getExperiment disableRelevancy
        let bound = env ^. boundTypeVars
        let rets = filter (isSubtypeOf bound tgt) (allTypesOf cover)
        liftIO (evalStateT (generateProgram sigs src args rets disrel) initialFormer)

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

    fillSketch firedTrans = do
        fm <- gets $ view functionMap
        src <- gets $ view sourceTypes
        nameMap <- gets $ view nameMapping
        repLists <- mapM getGroupRep firedTrans
        let reps = map head repLists
        let args = Map.keys $ foArgsOf env
        writeLog 2 "fillSketch" $ text "found path" <+> pretty firedTrans
        mapM_ (\f -> do
            let name = lookupWithError "nameMapping" f nameMap
            modify $ over useCount $ Map.insertWith (+) name 1) firedTrans
        let sigs = substPair $ substName firedTrans $ map (findFunction fm) reps
        writeLog 2 "fillSketch" $ text "found filtered sigs" <+> pretty sigs
        let initialFormer = FormerState HashMap.empty []
        withTime FormerTime $ generateCode initialFormer src args sigs

    checkUntilFail :: MonadIO m
                    => EncodeState
                    -> [[Id]]
                    -> PNSolver m (RProgram, EncodeState, [[Id]])
    checkUntilFail st' [] = findProgram env dst (st' {prevChecked=True}) []
    checkUntilFail st' (path:ps) = do
        writeLog 1 "checkUntilFail" $ pretty path
        codeResult <- fillSketch path
        checkResult <- withTime TypeCheckTime $
                        firstCheckedOrError $
                        sortOn (Data.Ord.Down . length) $
                        Set.toList codeResult
        rs <- getExperiment refineStrategy
        stop <- getExperiment stopRefine
        placeNum <- getExperiment threshold
        cover <- gets $ view abstractionCover
        case checkResult of
            Nothing -> checkUntilFail st' ps
            Just (Left code) -> do
                mbSln <- checkSolution st' code
                case mbSln of
                    Nothing -> checkUntilFail st' ps
                    Just p -> return (p, st' {prevChecked = null ps}, ps)
            Just (Right err)
                | not (doRefine rs) || (stop && coverSize cover >= placeNum) -> do
                    cover <- gets $ view abstractionCover
                    funcs <- gets $ view activeSigs
                    modify $ over solverStats (\s -> s {
                          numOfPlaces = Map.insert (iterations s + 1) (coverSize cover) (numOfPlaces s)
                        , numOfTransitions = Map.insert (iterations s + 1) (Set.size funcs) (numOfTransitions s)
                        })
                    checkUntilFail st' ps
                | otherwise -> do
                    cover <- gets $ view abstractionCover
                    funcs <- gets $ view activeSigs
                    nextSolution st' err

    firstCheckedOrError [] = return Nothing
    firstCheckedOrError [x] = Just <$> parseAndCheck x
    firstCheckedOrError (x:xs) = do
        res <- parseAndCheck x
        case res of
            Left prog -> return $ Just res
            Right err -> firstCheckedOrError xs

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
        counter <- gets $ view nameCounter
        writeLog 1 "parseAndCheck" $ text "Find program" <+> pretty (recoverNames mapping prog)
        (btm, checkerState) <- runStateT (bottomUpCheck env prog) $ emptyCheckerState {
                checkerNameCounter = counter,
                checkerNameMapping = mapping
            }
        writeLog 2 "parseAndCheck" $ text "bottom up checking get program" <+> pretty (recoverNames mapping btm)
        let checkStatus = isChecked checkerState
        let tyBtm = typeOf btm
        writeLog 2 "parseAndCheck" $ pretty tyBtm <+> pretty dst
        checkerState' <- if checkStatus
            then execStateT (solveTypeConstraint env tyBtm dst) checkerState
            else return checkerState
        let tass = typeAssignment checkerState'
        -- writeLog 1 "parseAndCheck" $ pretty tass
        modify $ set nameCounter (checkerNameCounter checkerState')
        if isChecked checkerState'
            then return (Left btm)
            else do
                let tyBtm' = toAbstractType $ typeSubstitute tass tyBtm
                let absDst = toAbstractType dst
                absBtm <- observeT $ pickGeneralization tyBtm' absDst
                -- writeLog 1 "parseAndCheck" $ text "get distinguished type" <+> pretty absBtm
                return (Right (btm, absBtm))

    nextSolution st (prog, at) = do
        cover <- gets $ view abstractionCover
        splitInfo <- withTime RefinementTime (refineSemantic env prog at)
        -- writeLog 1 "nextSolution" $ text "get split info" <+> pretty splitInfo
        -- add new places and transitions into the petri net
        cover <- gets $ view abstractionCover
        t2tr <- gets $ view type2transition
        modify $ over solverStats (\s ->
            s   { iterations = iterations s + 1
                , numOfPlaces =
                       Map.insert
                           (iterations s)
                           (coverSize cover)
                           (numOfPlaces s)
                , numOfTransitions =
                       Map.insert
                           (iterations s)
                           (transitionNb st)
                           (numOfTransitions s)
                })
        st' <- withTime EncodingTime (fixEncoder env dst st splitInfo)
        findProgram env dst st' []

    checkSolution st code = do
        solutions <- gets $ view currentSolutions
        mapping <- gets $ view nameMapping
        let code' = recoverNames mapping code
        disableDemand <- getExperiment disableDemand
        disableRele <- getExperiment disableRelevancy
        checkedSols <-
            withTime
                TypeCheckTime
                (filterM (liftIO . runGhcChecks (disableRele || disableDemand) env dst) [code'])
        if (code' `elem` solutions) || null checkedSols
            then return Nothing
            else return $ Just code'

findFirstN :: MonadIO m
            => Environment
            -> RType
            -> EncodeState
            -> [[Id]]
            -> Int
            -> PNSolver m ()
findFirstN env dst st ps n
    | n == 0 = return ()
    | otherwise = do
        (soln, st', ps') <- withTime TotalSearch $ findProgram env dst st ps
        writeSolution soln
        modify $ over currentSolutions ((:) soln)
        currentSols <- gets $ view currentSolutions
        writeLog 2 "findFirstN" $ text "Current Solutions:" <+> pretty currentSols
        findFirstN env dst st' ps' (n - 1)

runPNSolver :: MonadIO m => Environment -> RType -> PNSolver m ()
runPNSolver env t = do
    let args = map toFunDts (Map.elems (env ^. arguments))
    let foArgs = Map.filter (not . isFunctionType) (env ^. arguments)
    let nonArgSyms = Map.difference (env ^. symbols) foArgs
    let res = [t]
    modify $ set (explorer . forwardSet) (Set.fromList args)
    modify $ set (explorer . backwardSet) (Set.fromList res)
    modify $ set (explorer . nullaries) (Map.filter ((==) 0 . arity . toMonotype) nonArgSyms)
    withTime TotalSearch $ initNet env
    st <- withTime TotalSearch $ withTime EncodingTime (resetEncoder env t)
    cnt <- getExperiment solutionCnt
    findFirstN env t st [] cnt
    msgChan <- gets $ view messageChan
    liftIO $ writeChan msgChan (MesgClose CSNormal)

writeSolution :: MonadIO m => UProgram -> PNSolver m ()
writeSolution code = do
    stats <- gets $ view solverStats
    loc <- gets $ view currentLoc
    msgChan <- gets $ view messageChan
    let stats' = stats {pathLength = loc}
    liftIO $ writeChan msgChan (MesgP (code, stats'))
    liftIO $ printSolution code
    liftIO $ print stats'

{- helper functions -}
addCloneFunction :: MonadIO m => AbstractSkeleton -> PNSolver m Id
addCloneFunction ty = do
    let fname = show ty ++ "|clone"
    let fc = FunctionCode fname [] [ty] [ty, ty]
    modify $ over functionMap (HashMap.insert fname fc)
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
    mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps

updateSrcTgt :: MonadIO m
            => Environment
            -> RType
            -> PNSolver m ([AbstractSkeleton], AbstractSkeleton)
updateSrcTgt env dst = do
    -- reset source and destination types
    let binds = env ^. boundTypeVars
    abstraction <- gets (view abstractionCover)
    tgt <- currentAbst binds abstraction (toAbstractType (shape dst))
    modify $ set targetType tgt

    let foArgs = Map.filter (not . isFunctionType) (env ^. arguments)
    srcTypes <- mapM ( currentAbst binds abstraction
                     . toAbstractType) $ Map.elems foArgs
    modify $ set sourceTypes srcTypes
    noClone <- getExperiment disableCopy
    unless noClone $ mapM_ addCloneFunction (tgt:srcTypes)
    return (srcTypes, tgt)

type EncoderArgs = (Int
                    , HashMap Id [Id]
                    , [AbstractSkeleton]
                    , [FunctionCode]
                    , HashMap AbstractSkeleton (Set Id))

prepEncoderArgs :: MonadIO m
                => Environment
                -> AbstractSkeleton
                -> PNSolver m EncoderArgs
prepEncoderArgs env tgt = do
    cover <- gets $ view abstractionCover
    loc <- gets $ view currentLoc
    funcs <- gets $ view functionMap
    t2tr <- gets $ view type2transition
    musters <- gets $ view mustFirers
    let bound = env ^. boundTypeVars
    let accepts = superTypeOf bound cover tgt
    let rets = sortBy (compareAbstract bound) accepts
    let sigs = HashMap.elems funcs
    writeLog 3 "prepEncoderArgs" $ text "current must firers" <+> pretty (HashMap.toList musters)
    return (loc, musters, rets, sigs, t2tr)

foArgsOf :: Environment -> Map Id RType
foArgsOf = Map.filter (not . isFunctionType) . _arguments

noneInst instMap id t = not (HashMap.member (id, absFunArgs id t) instMap)

diffInst tvs instMap id t = let oldt = snd (fromJust $ HashMap.lookup (id, absFunArgs id t) instMap)
                             in t /= oldt && isSubtypeOf tvs (lastAbstract t) (lastAbstract oldt)

excludeUseless :: MonadIO m => Id -> AbstractSkeleton -> PNSolver m ()
excludeUseless id ty = do
    instMap <- gets (view instanceMapping)
    let (tid, ty') = fromJust (HashMap.lookup (id, absFunArgs id ty) instMap)
    writeLog 3 "excludeUseless" $ text "delete" <+> pretty tid <+> text "==>" <+> pretty id <+> text "::" <+> pretty ty'
    modify $ over toRemove ((:) tid)

getGroupRep :: MonadIO m => Id -> PNSolver m [Id]
getGroupRep name = do
    gr <- gets $ view groupRepresentative
    ngm <- gets $ view nameToGroup
    let argGps = maybeToList $ Map.lookup name ngm
    writeLog 3 "getGroupRep" $ text name <+> text "is contained in group" <+> pretty argGps
    let argRp = mapMaybe (`Map.lookup` gr) argGps
    return argRp

assemblePair :: AbstractSkeleton
            -> AbstractSkeleton
            -> AbstractSkeleton
assemblePair first secod | absFunArgs "fst" first == absFunArgs "snd" secod =
    let AFunctionT p f = first
        AFunctionT _ s = secod
     in AFunctionT p (AFunctionT f s)
assemblePair first second = error "fst and snd have different arguments"
