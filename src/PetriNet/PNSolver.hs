{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Types.Encoder hiding (varName, mustFirers)
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
        tree <- gets (view abstractionTree)
        let typs = Set.toList tree
        writeLog 3 "instantiate" $ text "Current abstract types:" <+> pretty typs
        sigs' <- Map.toList <$> mapM freshType sigs
        foldM (\acc -> (<$>) (acc ++) . uncurry (instantiateWith env typs)) [] sigs'

-- add Pair_match function as needed
instantiateWith :: MonadIO m => Environment -> [AbstractSkeleton] -> Id -> RType -> PNSolver m [(Id, AbstractSkeleton)]
-- skip "snd" function, it would be handled together with "fst"
instantiateWith env typs id t | id == "snd" = return []
instantiateWith env typs id t = do
    abstraction <- gets (view abstractionTree)
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
    noneInst instMap id t = not (HashMap.member (id, absFunArgs id t) instMap)

    diffInst instMap id t = snd (fromJust $ HashMap.lookup (id, absFunArgs id t) instMap) /= t

    assemblePair first secod | absFunArgs "fst" first == absFunArgs "snd" secod =
        let AFunctionT p f = first
            AFunctionT _ s = secod
         in AFunctionT p (AFunctionT f s)
    assemblePair first second = error "fst and snd have different arguments"

    enumSigs typ = do
        let bound = env ^. boundTypeVars
        -- [TODO] to support higher order function, this is incorrect
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
        -- add corresponding must firers, function code and type mapping
        when (Map.member id (env ^. arguments))
             (modify $ over mustFirers (HashMap.insertWith (++) id [newId]))
        modify $ over nameMapping (Map.insert newId id)
        modify $ over instanceMapping (HashMap.insert (id, absFunArgs id ty) (newId, ty))
        writeLog 4 "instantiateWith" $ text id <+> text "==>"  <+> text newId <+> text "::" <+> pretty ty
        return (newId, ty)

    -- We have refined the real function behind id and now th current instantiations aren't needed
    excludeUseless id ty = do
        instMap <- gets (view instanceMapping)
        let (tid, _) = fromJust (HashMap.lookup (id, absFunArgs id ty) instMap)
        -- writeLog 3 $ text tid <+> text "exists in the instance map for" <+> text id

        modify $ over toRemove ((:) tid)
        -- TODO: check if tid in groupElems: then remove from groupElems, do not add to toremove; else, add to toremove
        -- modify $ over toRemove ((:) tid)
        musts <- gets (view mustFirers)
        when (id `HashMap.member` musts)
             (modify $ over mustFirers (HashMap.insertWith (flip (\\)) id [tid]))



addSignatures :: MonadIO m => Environment -> PNSolver m (Map Id AbstractSkeleton)
addSignatures env = do
    let foArgs = foArgsOf env
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    let envSymbols = allSymbols env
    -- first order arguments are tokens but not transitions in petri net
    let usefulPipe k _ = k `notElem` Map.keys foArgs
    let usefulSymbols = Map.filterWithKey usefulPipe envSymbols
    sigs <- instantiate env usefulSymbols

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


-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m SplitInfo
refineSemantic env prog at = do
    -- back propagation of the error types to get all split information
    propagate env prog at
    -- get the split pairs
    splits <- gets (view splitTypes)
    writeLog 3 "refineSemantic splitTypes" $ pretty splits
    -- add new instantiated signatures into solver
    sigs <- addSignatures env
    -- get removed transitions
    (toAdd, removables) <- ifM (getExperiment coalesceTypes) (
      do
        removables' <- gets $ view toRemove
        foldM updateRemovable ([], []) removables'
        )
      ((gets $ view toRemove) >>= return . (,) [])
    writeLog 3 "refineSemantic (toAdd, removables)" $ pretty (toAdd, removables)
    if any (\x -> x `elem` removables) toAdd then error "trying to add and remove at the same time" else return ()
    modify $ over activeSigs (\as -> Set.union (Set.fromList toAdd) $ Set.difference as (Set.fromList removables))
    -- add clone functions and add them into new transition set
    cloneNames <- mapM addCloneFunction $ Set.toList splits
    -- call the refined encoder on these signatures and disabled signatures
    return SplitInfo { newPlaces = Set.toList splits
                     , removedTrans = removables
                     , newTrans = Map.keys sigs ++ cloneNames ++ toAdd
                     }
    where

        updateRemovable :: MonadIO m => ([Id], [Id]) -> Id -> PNSolver m ([Id], [Id])
        updateRemovable (toAdd, removables) tid = do
            nm <- gets $ view nameMapping
            imap <- gets $ view instanceMapping
            sigs <- gets $ view currentSigs
            t2t <- gets $ view type2transition
            fm <- gets $ view functionMap
            gm <- gets $ view groupMap
            grs <- gets $ view groupRepresentative
            t2g <- gets $ view typeToGroup
            writeLog 3 "updateRemovable" $ text "tid:" <+> text tid
            writeLog 3 "updateRemovable" $ text "tid in sig:" <+> (pretty $ Map.lookup tid sigs)
            writeLog 3 "updateRemovable" $ text "aty in groups:" <+> (pretty ((\k -> Map.lookup k t2g) <$> (Map.lookup tid sigs)))
            let Just (groupId, _) = find (\(_, ids) -> Set.member tid ids) $ Map.toList gm
            let isRepresentative = (lookupWithError "groupRepresentative" groupId grs) == tid
            -- let (groupId, mbRepresentative) = (\k -> (k, Map.lookup k grs)) ((tid `lookupWithError` sigs) `lookupWithError` t2g)
            writeLog 3 "updateRemovable" $ pretty (groupId, isRepresentative)
            let groupSize = Set.size $ lookupWithError "groupMap" groupId gm
            when (groupSize <= 1 && not isRepresentative) (error "the impossible has happened")
            if isRepresentative
                then do
                    let newgm = Map.update (removeFromSet tid) groupId gm
                    modify $ set groupMap newgm
                    -- Select representative here
                    let mbNewRepresentative = selectRepresentative <$> Map.lookup groupId newgm
                    let newgrs = Map.update (const mbNewRepresentative) groupId grs
                    writeLog 3 "updateRemovable isRepresentative" $ text $ printf "Replacing representative for %s: %s -> %s" groupId tid (show mbNewRepresentative)
                    modify $ set groupRepresentative newgrs
                    case mbNewRepresentative of
                        Nothing -> return (toAdd, tid:removables)
                        Just (newRep) -> do
                            -- using the abstract type of tid, since both tid and the new representative should
                            -- have the same abstract type.
                            let abstractType = lookupWithError "currentSigs" tid sigs
                            addEncodedFunction (newRep, abstractType)
                            return $ (newRep:toAdd, tid:removables)
                else do
                    modify $ over groupMap $ Map.update (removeFromSet tid) groupId
                    return (toAdd, removables)

        removeFromSet :: Id -> Set Id -> Maybe (Set Id)
        removeFromSet tid ids = let
            ids' = Set.delete tid ids
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
    transitions <- gets (HashMap.elems . view type2transition)
    writeLog 3 "initNet" $ text "allTys: " <+> pretty allTy
    mapM_ addCloneFunction (filter (not . isAFunctionT) allTy)
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
    let AFunctionT ret (AFunctionT arg0 arg1) = f
    -- add fst and snd sigs
    modify $ over currentSigs (Map.insert (replaceId "pair_match" "fst" id) (AFunctionT ret arg0))
    modify $ over currentSigs (Map.insert (replaceId "pair_match" "snd" id) (AFunctionT ret arg1))
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
    (loc, musters, rets, funcs, tid2tr) <- prepEncoderArgs env tgt
    let srcStr = map show srcTypes
    liftIO $ encoderInit loc musters srcStr rets funcs tid2tr

incEncoder :: MonadIO m => Environment -> EncodeState -> PNSolver m EncodeState
incEncoder env st = do
    tgt <- gets (view targetType)
    src <- gets (view sourceTypes)
    (_, _, rets, funcs, _) <- prepEncoderArgs env tgt
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
            args <- gets $ view paramNames
            let sortedRes = sortOn snd res
            let transNames = map fst sortedRes
            writeLog 2 "findPath" $ text "found path" <+> pretty transNames
            let usefulTrans = filter skipClone transNames
            let sigNames = map removeSuffix usefulTrans
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
        cover <- gets (view abstractionTree)
        let bound = env ^. boundTypeVars
        let rets = filter (isSubtypeOf bound tgt) (Set.toList cover)
        liftIO (evalStateT (generateProgram sigs src args (map show rets) True) initialFormer)

    skipUncolor = not . isInfixOf "|uncolor"
    skipClone = not . isInfixOf "|clone"
    skipDiscard = not . isInfixOf "|discard"
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
    abstraction <- gets (view abstractionTree)
    writeLog 2 "fixEncoder" $ text "new abstraction cover is" <+> pretty (Set.toList abstraction)
    (srcTypes, tgt) <- updateSrcTgt env dst
    writeLog 2 "fixEncoder" $ text "fixed parameter types are" <+> pretty srcTypes
    writeLog 2 "fixEncoder" $ text "fixed return type is" <+> pretty tgt
    writeLog 3 "fixEncoder" $ text "get split information:" </> pretty info
    let newTyps = newPlaces info
    mapM_ addCloneFunction (filter (not . isAFunctionT) newTyps)
    modify $ over type2transition (HashMap.filter (not . null))

    (loc, musters, rets, funcs, tid2tr) <- prepEncoderArgs env tgt
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
         funcs <- gets (view activeSigs)
         cover <- gets (view abstractionTree)
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
        cover <- gets (view abstractionTree)
        if stop && Set.size cover >= placeNum
           then findProgram env dst (st {prevChecked=True})
           else do
             splitInfo <- withTime RefinementTime (refineSemantic env prog at)
             -- add new places and transitions into the petri net
             res <- refine st splitInfo
             cover <- gets (view abstractionTree)
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

addHoParam t = do
    let addTransition k tid = HashMap.insertWith (\_ y -> nubOrd $ tid:y) k [tid]
    let params = init (nub (decomposeHo t))
    let ret = last (nub (decomposeHo t))
    let uncolorTr = show t ++ "|uncolor"
    modify $ over type2transition (addTransition ret uncolorTr)
    modify $ over type2transition (addTransition t uncolorTr)

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
    abstraction <- gets (view abstractionTree)
    tgt <- currentAbst binds abstraction (toAbstractType (shape dst))
    modify $ set targetType tgt

    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    srcTypes <- mapM ( currentAbst binds abstraction
                     . toAbstractType
                     . shape
                     . toMonotype) $ Map.elems foArgs
    modify $ set sourceTypes srcTypes
    modify $ set paramNames $ Map.keys foArgs
    return (srcTypes, tgt)

type EncoderArgs = (Int, HashMap Id [Id], [Id], [FunctionCode], HashMap Id [Id])

prepEncoderArgs :: MonadIO m => Environment -> AbstractSkeleton -> PNSolver m EncoderArgs
prepEncoderArgs env tgt = do
    abstraction <- gets (view abstractionTree)
    loc <- gets $ view currentLoc
    funcs <- gets $ view functionMap
    t2tr <- gets $ view type2transition
    musters <- gets $ view mustFirers
    let bound = env ^. boundTypeVars
    let tid2tr = HashMap.foldrWithKey (\k v -> HashMap.insert (show k) v) HashMap.empty t2tr
    let accepts = filter (isSubtypeOf bound tgt) (Set.toList abstraction)
    let rets = sortBy (compareAbstract bound) accepts
    let strRets = map show rets
    let sigs = HashMap.elems funcs
    return (loc, musters, strRets, sigs, tid2tr)

foArgsOf :: Environment -> Map Id RSchema
foArgsOf = Map.filter (not . isFunctionType . toMonotype) . _arguments
