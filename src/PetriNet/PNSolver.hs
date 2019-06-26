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
encodeFunction id t@(AFunctionT tArg tRet) = FunctionCode id hoParams params [show $ lastAbstractType t]
  where
    base = (0, [])
    hoFun x = encodeFunction (show x) x
    hoParams = map hoFun $ filter isAFunctionT (abstractParamList t)
    params = map show (abstractParamList t)
encodeFunction id t@AScalar {} = FunctionCode id [] [] [show t]

instantiate :: MonadIO m => Environment -> Map Id RSchema -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs = do
    modify $ set toRemove []
    Map.fromList <$> instantiate' sigs
  where
    instantiate' sigs = do
        tree <- gets (view abstractionCover)
        let typs = Set.toList tree
        writeLog 3 "instantiate" $ text "Current abstract types:" <+> pretty typs
        sigs' <- Map.toList <$> mapM freshType sigs
        foldM (\acc -> (<$>) (acc ++) . uncurry (instantiateWith env typs)) [] sigs'

-- add Pair_match function as needed
instantiateWith :: MonadIO m => Environment -> [AbstractSkeleton] -> Id -> RType -> PNSolver m [(Id, AbstractSkeleton)]
-- skip "snd" function, it would be handled together with "fst"
instantiateWith env typs id t | id == "snd" = return []
instantiateWith env typs id t = do
    abstraction <- gets (view abstractionCover)
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
           let matches' = filter (\t -> noneInst instMap "pair_match" t || diffInst instMap "pair_match" t) matches
           mapM (mkNewSig "pair_match") matches'
       else do
           ft <- freshType (Monotype t)
           let t' = toAbstractType (shape ft)
           rawSigs <- enumSigs t'
           let sigs = filter (\t -> noneInst instMap id t || diffInst instMap id t) rawSigs
           mapM (mkNewSig id) sigs
  where

    assemblePair first secod | absFunArgs "fst" first == absFunArgs "snd" secod =
        let AFunctionT p f = first
            AFunctionT _ s = secod
         in AFunctionT p (AFunctionT f s)
    assemblePair first second = error "fst and snd have different arguments"

    enumSigs typ = do
        let bound = env ^. boundTypeVars
        let argNum = length (decompose typ) - 1
        let allArgCombs = multiPermutation argNum typs
        applyRes <- mapM (applySemantic bound typ) allArgCombs
        let resSigs = filter (not . isBot . fst) (zip applyRes allArgCombs)
        return $ map (uncurry $ foldr AFunctionT) resSigs

    mkNewSig id ty = do
        instMap <- gets (view instanceMapping)
        newId <- if "pair_match" `isPrefixOf` id then freshId (id ++ "_")
                                                 else freshId "f"
        -- when same arguments exist for a function, replace it
        unless (noneInst instMap id ty) (excludeUseless id ty)
        modify $ over nameMapping (Map.insert newId id)
        modify $ over instanceMapping (HashMap.insert (id, absFunArgs id ty) (newId, ty))
        writeLog 3 "instantiateWith" $ text id <+> text "==>"  <+> text newId <+> text "::" <+> pretty ty
        return (newId, ty)

addHoArg :: MonadIO m => Map Id AbstractSkeleton -> [Id] -> Bool -> Id -> PNSolver m (Map Id AbstractSkeleton)
addHoArg sigs tvs queryArg id = do
    nameMap <- gets $ view nameMapping
    let isHoInstance k n = id == n && not ((id ++ "ho") `isPrefixOf` k)
    let names = Map.keys $ Map.filterWithKey isHoInstance nameMap
    sigMbs <- mapM addOrRemoveHo names
    let sigs' = Map.fromList $ map fromJust $ filter isJust sigMbs
    let hoNames = Map.keys sigs'
    mapM_ (\n -> modify $ over nameMapping $ Map.insert n id) hoNames
    return sigs'
  where
    addOrRemoveHo name = do
      writeLog 3 "addHoArg" $ text "checking" <+> text name <+> text "for" <+> text id
      instMap <- gets $ view instanceMapping
      cover <- gets $ view abstractionCover
      currSigs <- gets $ view currentSigs
      let sigs' = currSigs `Map.union` sigs
      let mkCompact = currentAbst tvs cover . compactAbstractType . fromJust
      groupName <- getGroupRep name
      let name' = if null groupName then name else head groupName
      t <- mkCompact (Map.lookup name' sigs')
      writeLog 3 "addHoArg" $ text "checking" <+> text name <+> text "::" <+> pretty t
      if noneInst instMap id t || diffInst instMap id t
        then do
          unless (noneInst instMap id t) (excludeUseless id t)
          f <- freshId $ id ++ "ho"
          let args = absFunArgs id t
          modify $ over instanceMapping (HashMap.insert (id, args) (f, t))
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
    hoSigs <- mapM (addHoArg envSigs binds False) cands
    hoSigs' <- mapM (addHoArg envSigs binds True) $ Map.keys hoArgs
    let sigs = Map.unions $ envSigs : hoSigs ++ hoSigs'

    sigs' <- ifM (getExperiment coalesceTypes) (
      do
        (t2g, sigGroups) <- groupSignatures sigs
        -- Do I want to take the sigs here?
        let representatives = Map.map selectRepresentative sigGroups
        let sigs' = Map.restrictKeys sigs (Set.fromList $ Map.elems representatives)
        modify $ over groupRepresentative $ Map.union representatives
        modify $ over groupMap (updateGroups sigGroups)
        modify $ over typeToGroup (Map.union t2g)
        return sigs'
        )
      (return sigs)
    modify $ over activeSigs (Set.union (Map.keysSet sigs'))
    mapM_ addEncodedFunction (Map.toList sigs')
    return sigs'
    where
        -- Given the new set of groups, update the current group mapping
        updateGroups :: Map Id (Set Id) -> Map Id (Set Id) -> Map Id (Set Id)
        updateGroups sigGroups gm = foldr updategm gm (Map.toList sigGroups)
        updategm (groupName, groupMembers) = Map.insertWith Set.union groupName groupMembers

selectRepresentative = Set.elemAt 0

addMusters :: MonadIO m => Id -> PNSolver m ()
addMusters arg = do
    nameMap <- gets $ view nameMapping
    let argFuncs = Map.keys $ Map.filter (arg ==) nameMap
    argRps <- mapM getGroupRep argFuncs
    modify $ over mustFirers (HashMap.insertWith union arg $ concat argRps)

-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m SplitInfo
refineSemantic env prog at = do
    -- back propagation of the error types to get all split information
    propagate env prog $ compactAbstractType at
    -- get the split pairs
    splits <- gets (view splitTypes)
    writeLog 3 "refineSemantic splitTypes" $ pretty splits
    -- add new instantiated signatures into solver
    sigs <- addSignatures env
    -- get removed transitions
    (toAdd, removables) <- ifM (getExperiment coalesceTypes) (
      do
        removables' <- gets $ view toRemove
        splitGroups removables'
        )
      ((gets $ view toRemove) >>= return . (,) [])
    let addedSigs = map fst toAdd
    writeLog 3 "refineSemantic (toAdd, removables)" $ pretty (addedSigs, removables)
    if any (\x -> (fst x) `elem` removables) toAdd then error "trying to add and remove at the same time" else return ()
    mapM addEncodedFunction toAdd

    modify $ over activeSigs (\as -> Set.union (Set.fromList addedSigs) $ Set.difference as (Set.fromList removables))
    modify $ over mustFirers (HashMap.map (filter (`notElem` removables)))
    -- add clone functions and add them into new transition set
    cloneNames <- mapM addCloneFunction $ Set.toList splits
    -- update the higer order query arguments
    let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    mapM_ addMusters (Map.keys hoArgs)
    -- call the refined encoder on these signatures and disabled signatures
    return SplitInfo { newPlaces = Set.toList splits
                     , removedTrans = removables
                     , newTrans = Map.keys sigs ++ cloneNames ++ addedSigs
                     }
    where

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
            return (toAdd, toRemove)

        updateRepresentatives :: MonadIO m => ([(Id, AbstractSkeleton)], [Id], Map GroupId Id) -> (GroupId, Id) -> PNSolver m ([(Id, AbstractSkeleton)], [Id], Map GroupId Id)
        updateRepresentatives (addables, removables, newReps) (gid, rep)= do
            gm <- gets $ view groupMap
            sigs <- gets $ view currentSigs
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
                                    let newRep = selectRepresentative currentGroup
                                    let abstractType = lookupWithError "currentSigs" rep sigs
                                    let addables' = (newRep, abstractType):addables
                                    return (addables', removables', Map.insert gid newRep newReps)
                                -- There was only that one element in the group, so with the leader gone, the group goes away.
                                else return (addables, removables', newReps)
                        -- after all the removals, the current representative is still in its original group
                        else return (addables, removables, Map.insert gid rep newReps)

        removeFromSet :: Id -> Set Id -> Maybe (Set Id)
        removeFromSet tid ids = let
            ids' = Set.delete tid ids
            in if (Set.size ids' == 0) then Nothing else Just ids'

        shrinkSet :: Set Id -> Set Id -> Maybe (Set Id)
        shrinkSet toRemove ids = let
            ids' = Set.difference ids toRemove
            in if (Set.size ids' == 0) then Nothing else Just ids'


initNet :: MonadIO m => Environment -> PNSolver m ()
initNet env = withTime ConstructionTime $ do
    -- reset the solver state
    modify $ set functionMap HashMap.empty
    modify $ set currentSigs Map.empty
    modify $ set type2transition HashMap.empty
    modify $ set instanceMapping HashMap.empty

    addSignatures env
    -- add clone functions for each type
    allTy <- gets (HashMap.keys . view type2transition)
    mapM_ addCloneFunction (filter (not . isAFunctionT) allTy)
    -- add higher order query arguments
    let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    mapM_ addMusters (Map.keys hoArgs)
  where
    abstractSymbol id sch = do
        t <- freshType sch
        let absTy = toAbstractType (shape t)
        return (id, absTy)

addEncodedFunction :: MonadIO m => (Id, AbstractSkeleton) -> PNSolver m ()
addEncodedFunction (id, f) | "pair_match" `isPrefixOf` id = do
    let toMatch (FunctionCode name ho [p1,p2] ret) = FunctionCode name ho [p1] (p2:ret)
    let ef = toMatch $ encodeFunction id f
    modify $ over functionMap (HashMap.insert id ef)
    modify $ over currentSigs (Map.insert id f)
    -- store the used abstract types and their groups into mapping
    updateTy2Tr id f
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
    let srcStr = map show srcTypes  
    liftIO $ encoderInit loc musters srcStr rets funcs tid2tr incremental

incEncoder :: MonadIO m => Environment -> EncodeState -> PNSolver m EncodeState
incEncoder env st = do
    tgt <- gets (view targetType)
    src <- gets (view sourceTypes)
    (_, _, rets, funcs, _, _) <- prepEncoderArgs env tgt
    liftIO $ execStateT (encoderInc funcs (map show src) rets) st

findPath :: (MonadIO m) => Environment -> RType -> EncodeState -> PNSolver m (CodePieces, EncodeState)
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
        _  -> withTime FormerTime $ do
            fm <- gets $ view functionMap
            src <- gets $ view sourceTypes
            let args = Map.keys $ foArgsOf env
            let firedTrans = res
            writeLog 2 "findPath" $ text "found path" <+> pretty firedTrans
            let sigNames = filter skipClone firedTrans
            -- dsigs <- gets $ view detailedSigs
            -- let sigNames' = filter (\name -> Set.member name dsigs || "pair_match" `isPrefixOf` name) sigNames
            let sigs = substPair (map (findFunction fm) sigNames)
            writeLog 2 "findPath" $ text "found filtered sigs" <+> text (show sigs)
            let initialFormer = FormerState 0 HashMap.empty [] []
            code <- generateCode initialFormer (map show src) args sigs
            return (code, st')
  where
    findFunctions fm groups name =
        case Map.lookup name groups of
            Just g -> findFunction fm (head g)
            Nothing -> error $ "cannot find function group " ++ name

    findFunction fm name = fromMaybe (error $ "cannot find function name " ++ name)
                                     (HashMap.lookup name fm)

    combinations []    = []
    combinations [x]   = [x]
    combinations (h:t) = [ hh:tt | hh <- h, tt <- combinations t ]

    generateCode initialFormer src args sigs = do
        tgt <- gets (view targetType)
        cover <- gets (view abstractionCover)
        let bound = env ^. boundTypeVars
        let rets = filter (isSubtypeOf bound tgt) (Set.toList cover)
        liftIO (evalStateT (generateProgram sigs src args (map show rets)) initialFormer)

    skipClone = not . isInfixOf "|clone"
    removeSuffix = removeLast '|'

    substPair [] = []
    substPair (x:xs) = if "pair_match" `isPrefixOf` funName x
                          then   ( x { funName = replaceId "pair_match" "fst" (funName x), funReturn = [head (funReturn x)] } )
                               : ( x { funName = replaceId "pair_match" "snd" (funName x), funReturn = [funReturn x !! 1] } )
                               : substPair xs
                          else x : substPair xs

fixEncoder :: MonadIO m => Environment -> RType -> EncodeState -> SplitInfo -> PNSolver m EncodeState
fixEncoder env dst st info = do
    let binds = env ^. boundTypeVars
    abstraction <- gets (view abstractionCover)
    writeLog 2 "fixEncoder" $ text "new abstraction cover is" <+> pretty (Set.toList abstraction)
    (srcTypes, tgt) <- updateSrcTgt env dst
    writeLog 2 "fixEncoder" $ text "fixed parameter types are" <+> pretty srcTypes
    writeLog 2 "fixEncoder" $ text "fixed return type is" <+> pretty tgt
    writeLog 3 "fixEncoder" $ text "get split information" </> pretty info
    modify $ over type2transition (HashMap.filter (not . null))
    (loc, musters, rets, funcs, tid2tr, _) <- prepEncoderArgs env tgt
    liftIO $ execStateT (encoderRefine info musters (map show srcTypes) rets funcs tid2tr) st

findProgram :: MonadIO m => Environment -> RType -> EncodeState -> PNSolver m (RProgram, EncodeState)
findProgram env dst st = do
    modify $ set splitTypes Set.empty
    modify $ set typeAssignment Map.empty
    writeLog 2 "findProgram" $ text "calling findProgram"
    (codeResult, st') <- findPath env dst st
    writeLog 2 "findProgram" $ pretty (Set.toList codeResult)
    checkResult <- withTime TypeCheckTime (firstCheckedOrError $ sortOn length (Set.toList codeResult))
    rs <- getExperiment refineStrategy
    if isLeft checkResult
       then let Left code = checkResult in checkSolution st' code
       else do
         let Right err = checkResult 
         cover <- gets (view abstractionCover)
         funcs <- gets (view activeSigs)
         modify $ over solverStats (\s -> s {
            iterations = iterations s + 1
          , numOfPlaces = Map.insert (iterations s + 1) (Set.size cover) (numOfPlaces s)
          , numOfTransitions = Map.insert (iterations s + 1) (Set.size funcs) (numOfTransitions s)
         })
         nextSolution st' rs err
  where
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

    nextSolution st rs (prog, at) | not (doRefine rs) = do
        let st' = st { prevChecked = True }
        findProgram env dst st'
    nextSolution st _ (prog, at) = do
        stop <- getExperiment stopRefine
        placeNum <- getExperiment threshold
        cover <- gets (view abstractionCover)
        if stop && Set.size cover >= placeNum
           then findProgram env dst (st {prevChecked=True})
           else do
             splitInfo <- withTime RefinementTime (refineSemantic env prog at)
             -- add new places and transitions into the petri net
             res <- refine st splitInfo
             cover <- gets (view abstractionCover)
             funcs <- gets (view activeSigs)
             modify $ over solverStats (\s -> s {
                numOfPlaces = Map.insert (iterations s) (Set.size cover) (numOfPlaces s)
              , numOfTransitions = Map.insert (iterations s) (Set.size funcs) (numOfTransitions s)
             })
             return res

    refine st info = do
        st' <- withTime EncodingTime (fixEncoder env dst st info)
        findProgram env dst st'

    checkSolution st code = do
        let st' = st { prevChecked = True }
        solutions <- gets $ view currentSolutions
        mapping <- gets $ view nameMapping
        let code' = recoverNames mapping code
        disableDemand <- getExperiment disableDemand
        checkedSols <- withTime TypeCheckTime (filterM (\u -> liftIO (runGhcChecks disableDemand env dst u )) [code'])
        if (code' `elem` solutions) || (null checkedSols)
           then findProgram env dst st'
           else do
               modify $ over currentSolutions ((:) code')
               return (code', st')

printSolution solution = do
    liftIO $ putStrLn "*******************SOLUTION*********************"
    liftIO $ putStrLn $ "SOLUTION: " ++ mkOneLine (show solution)
    liftIO $ putStrLn "************************************************"


findFirstN :: (MonadIO m) => Environment -> RType -> EncodeState -> Int -> PNSolver m ()
findFirstN env dst st cnt | cnt == 1  = do
    (res, _) <- withTime TotalSearch $ findProgram env dst st
    stats <- gets $ view solverStats
    depth <- gets $ view currentLoc
    msgChan <- gets $ view messageChan
    strategy <- getExperiment refineStrategy
    writeLog 1 "findFirstN" $ text (show depth)
    let stats' = stats{pathLength = depth}
    printSolution res
    writeLog 2 "findFirstN" $ text (show stats)
    if noGarTyGarIdx strategy >= 0
      then do
        resetTiming
        modify $ set (searchParams . refineStrategy) NoGar
        modify $ set (searchParams . stopRefine) False
        modify $ set currentLoc 1
        modify $ set currentSolutions []
        runPNSolver env cnt dst
      else liftIO $ writeChan msgChan (MesgP (res, stats'))
findFirstN env dst st cnt = do
    (res, st') <- withTime TotalSearch $ findProgram env dst st
    msgChan <- gets $ view messageChan
    stats <- gets $ view solverStats
    loc <- gets $ view currentLoc
    let stats' = stats{pathLength = loc}
    printSolution res
    liftIO $ writeChan msgChan (MesgP (res, stats'))
    resetTiming
    findFirstN env dst st' (cnt-1)

runPNSolver :: MonadIO m => Environment -> Int -> RType -> PNSolver m ()
runPNSolver env cnt t = do
    writeLog 3 "runPNSolver" $ text $ show (allSymbols env)
    initNet env
    st <- withTime TotalSearch $ withTime EncodingTime (resetEncoder env t)
    findFirstN env t st cnt
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
addCloneFunction ty = do
    let tyStr = show ty
    let fname = tyStr ++ "|clone"
    let fc = FunctionCode fname [] [tyStr] [tyStr, tyStr]
    let addTransition k tid = HashMap.insertWith (\[x] y -> nubOrd $ x:y) k [tid]
    modify $ over functionMap (HashMap.insert fname fc)
    -- modify $ over currentSigs (Map.insert fname fc)
    modify $ over type2transition (addTransition ty fname)
    return fname

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
    let addTransition k tid = HashMap.insertWith (\[x] y -> nubOrd $ x:y) k [tid]
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

type EncoderArgs = (Int, HashMap Id [Id], [Id], [FunctionCode], HashMap Id [Id], Bool)

prepEncoderArgs :: MonadIO m => Environment -> AbstractSkeleton -> PNSolver m EncoderArgs
prepEncoderArgs env tgt = do
    abstraction <- gets (view abstractionCover)
    loc <- gets $ view currentLoc
    funcs <- gets $ view functionMap
    t2tr <- gets $ view type2transition
    musters <- gets $ view mustFirers
    incremental <- getExperiment incrementalSolving
    let bound = env ^. boundTypeVars
    let tid2tr = HashMap.foldrWithKey (\k v -> HashMap.insert (show k) v) HashMap.empty t2tr
    let accepts = filter (isSubtypeOf bound tgt) (Set.toList abstraction)
    let rets = sortBy (compareAbstract bound) accepts
    let strRets = map show rets
    let sigs = HashMap.elems funcs
    writeLog 3 "prepEncoderArgs" $ text "current must firers" <+> pretty (HashMap.toList musters)
    return (loc, musters, strRets, sigs, tid2tr, incremental)
    
foArgsOf :: Environment -> Map Id RSchema
foArgsOf = Map.filter (not . isFunctionType . toMonotype) . _arguments

noneInst instMap id t = not (HashMap.member (id, absFunArgs id t) instMap)

diffInst instMap id t = snd (fromJust $ HashMap.lookup (id, absFunArgs id t) instMap) /= t

excludeUseless :: MonadIO m => Id -> AbstractSkeleton -> PNSolver m ()
excludeUseless id ty = do
    instMap <- gets (view instanceMapping)
    let (tid, _) = fromJust (HashMap.lookup (id, absFunArgs id ty) instMap)
    modify $ over toRemove ((:) tid)

getGroupRep :: MonadIO m => Id -> PNSolver m [Id]
getGroupRep name = do
    gm <- gets $ view groupMap
    gr <- gets $ view groupRepresentative
    let withinGroup = Set.member name
    let argGps = Map.keys $ Map.filter withinGroup gm
    writeLog 3 "getGroupRep" $ text name <+> text "is contained in group" <+> pretty argGps
    let argRp = map fromJust $ filter isJust $ map (`Map.lookup` gr) argGps
    return argRp
