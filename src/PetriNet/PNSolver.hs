{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module PetriNet.PNSolver (runPNSolver) where

import           Control.Concurrent.Chan
import           Control.Lens hiding (backwards)
import           Control.Monad.Logic
import           Control.Monad.State
import qualified Data.Char                    as Char
import           Data.Data                    (Data)
import           Data.Either                  hiding (fromLeft, fromRight)
import           Data.Foldable
import           Data.Function                (on)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.List
import           Data.List.Extra
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Tuple
import           Debug.Trace
import           Language.Haskell.Exts.Parser (ParseResult (..), parseExp)
import           System.Console.ANSI
import           System.IO
import           Text.Printf

import           Database.Convert
import           Database.Generate
import           Database.Util
import           HooglePlus.Abstraction
import           HooglePlus.BiExplorer
import           HooglePlus.CodeFormer
import           HooglePlus.HyperExplorer
import           HooglePlus.Refinement
import           HooglePlus.Stats
import           HooglePlus.TypeChecker
import           PetriNet.AbstractType
import           PetriNet.GHCChecker
import           PetriNet.PNEncoder
import           PetriNet.Util
import           Synquid.Error
import           Synquid.Logic                hiding (varName)
import           Synquid.Parser               (parseFromFile, parseProgram,
                                               toErrorMessage)
import           Synquid.Pretty
import           Synquid.Program
import           Synquid.Type
import           Synquid.Util
import           Types.Abstract
import           Types.Checker
import           Types.Common
import           Types.Encoder                hiding (incrementalSolving,
                                               mustFirers, varName)
import           Types.Environment
import           Types.Experiments
import           Types.Program
import           Types.Solver
import           Types.Type
import           Types.HyperGraph

selection = False

instantiate :: MonadIO m => Environment -> Map Id RSchema -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs = do
    setSolver toRemove []
    noBlack <- getExperiment disableBlack
    blacks <- liftIO $ readFile "config/blacklist.txt"
    let sigs' = if noBlack then sigs else Map.withoutKeys sigs (Set.fromList $ words blacks)
    Map.fromList <$> instantiate' sigs'
  where
    instantiate' sigs = do
        tree <- getSolver abstractionCover
        let typs = allTypesOf tree
        writeLog 2 "instantiate" $ text "Current abstract types:" <+> text (show tree)
        sigs' <- lift $ Map.toList <$> mapM freshType sigs
        foldM (\acc -> (<$>) (acc ++) . uncurry (instantiateWith env typs)) [] sigs'

-- add Pair_match function as needed
instantiateWith :: MonadIO m => Environment -> [AbstractSkeleton] -> Id -> RType -> PNSolver m [(Id, AbstractSkeleton)]
-- skip "snd" function, it would be handled together with "fst"
instantiateWith env typs id t | id == "snd" = return []
instantiateWith env typs id t = do
    instMap <- getSolver instanceMapping
    let bound = env ^. boundTypeVars
    ft <- lift $ freshType (Monotype t)
    let t' = toAbstractType (shape ft)
    rawSigs <- enumSigs t'
    let sigs = filter (\t -> noneInst instMap id t || diffInst bound instMap id t) rawSigs
    sigs' <- mapM (mkNewSig id) sigs
    removes <- getSolver toRemove
    return $ filter ((`notElem` removes) . fst) sigs'
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
    instMap <- getSolver instanceMapping
    newId <- lift $ freshId "f"
    -- when same arguments exist for a function, replace it
    unless (noneInst instMap id ty) (excludeUseless id ty)
    overSolver nameMapping (Map.insert newId id)
    overSolver instanceCounts (HashMap.insertWith (+) id 1 )
    overSolver instanceMapping (HashMap.insert (id, sort $ absFunArgs id ty) (newId, ty))
    overSolver currentSigs (Map.insert newId ty)
    writeLog 3 "mkNewSig" $ text id <+> text "==>"  <+> text newId <+> text "::" <+> pretty ty
    return (newId, ty)

splitTransition :: MonadIO m => Environment -> AbstractSkeleton -> Id -> PNSolver m [(Id, AbstractSkeleton)]
splitTransition env newAt fid = do
    sigs <- getSolver currentSigs
    let ty = lookupWithError "currentSigs" fid sigs
    writeLog 3 "splitTransition" $ text "split transtion" <+> text fid <+> text "::" <+> pretty ty
    allSubsts ty
  where
    allSubsts ty = allSubsts' (lastAbstract ty) (absFunArgs fid ty)

    allSubsts' ret args = do
        cover <- getSolver abstractionCover
        nameMap <- getSolver nameMapping
        splits <- getSolver splitTypes
        instMap <- getSolver instanceMapping
        let parents = HashMap.keys $ HashMap.filter (Set.member newAt) cover
        let args' = enumArgs parents args
        writeLog 3 "allSubsts'" $ pretty args'
        funType <- lift $ freshType $ findSymbol nameMap env fid
        writeLog 3 "allSubsts'" $ text fid <+> text "::" <+> pretty funType
        let absFunType = toAbstractType (shape funType)
        let tvs = env ^. boundTypeVars
        rets <- mapM (applySemantic tvs absFunType) args'
        writeLog 3 "allSubsts'" $ text fid <+> text "returns" <+> pretty rets
        let validFunc (a, r) = not (equalAbstract tvs ret r && sort a == sort args) && not (isBot r)
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

addSignatures :: MonadIO m => Environment -> Map Id RSchema -> PNSolver m (Map Id FunctionCode)
addSignatures env usefulSymbols = do
    -- writeLog 2 "addSignatures" $ text "HI, we are here"
    t2id <- getSolver typeToIndex
    sigs <- instantiate env usefulSymbols
    let allTy = nub $ concatMap decompose (Map.elems sigs)
    -- writeLog 2 "addSignatures" $ pretty allTy
    writeLog 2 "addSignatures" $ pretty t2id
    let newTy = filter (not . (`Map.member` t2id)) allTy
    writeLog 2 "addSignatures" $ pretty newTy
    mapM_ (\t -> overSolver typeToIndex (\m -> Map.insert t (Map.size m) m)) newTy
    t2id <- getSolver typeToIndex
    let sigs' = Map.mapWithKey (encodeFunction t2id) sigs
    sigs'' <- ifM (getExperiment coalesceTypes) (mkGroups env sigs) (return sigs')
    mapM_ addEncodedFunction (Map.toList sigs'')
    return sigs''

mkGroups :: MonadIO m => Environment -> Map Id AbstractSkeleton -> PNSolver m (Map Id FunctionCode)
mkGroups env sigs = do
    t2id <- getSolver typeToIndex
    let encodedSigs = Map.mapWithKey (encodeFunction t2id) sigs
    (groupSigs, sigGroups) <- groupSignatures encodedSigs
    mapM_ updateGroups (Map.toList sigGroups)
    writeLog 2 "mkGroups" $ pretty groupSigs
    return groupSigs
  where
    selectOurRep hoAlias (gid, group) = (,) gid <$> selectRepresentative hoAlias gid group
    -- Given the new set of groups, update the current group mapping
    -- updateGroups :: Map Id (Set Id) -> Map Id (Set Id) -> Map Id (Set Id)
    -- updateGroups sigGroups gm = foldr updategm gm (Map.toList sigGroups)
    -- updategm (groupName, groupMembers) = Map.insertWith Set.union groupName groupMembers
    updateGroups (gid, fids) = do
        overSolver groupMap (Map.insertWith Set.union gid fids)
        mapM_ (\fid -> overSolver nameToGroup (Map.insert fid gid)) fids

selectRepresentative :: MonadIO m => Set Id -> GroupId -> Set Id -> PNSolver m Id
selectRepresentative hoArgs gid s = do
    let setToPickFrom = s
    strat <- getExperiment coalesceStrategy
    case strat of
        First             -> return $ Set.elemAt 0 setToPickFrom
        LeastInstantiated -> pickReprOrder sortOn setToPickFrom
        MostInstantiated  -> pickReprOrder sortDesc setToPickFrom
    where
        pickReprOrder sorting setToPickFrom = do
            nm <- getSolver nameMapping
            instCounts <- getSolver instanceCounts
            let idToCount id = instCounts HashMap.! (nm Map.! id)
            let countMapping = sorting snd $ map (\x -> (x, idToCount x)) $ Set.toList setToPickFrom
            writeLog 3 "SelectRepresentative" $ text gid <+> "needs to pick: " <+> pretty countMapping
            return $ fst $ head countMapping

        sortDesc f = sortBy (on (flip compare) f)


addMusters :: MonadIO m => Id -> PNSolver m ()
addMusters arg = do
    nameMap <- getSolver nameMapping
    n2g <- getSolver nameToGroup
    let eqArg n = n == arg || n == arg ++ hoPostfix
    let argFuncs = Map.keys $ Map.filter eqArg nameMap
    let argRps = map (n2g Map.!) argFuncs
    overSolver mustFirers (HashMap.insert arg argRps)

-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m SplitInfo
refineSemantic env prog at = do
    cover <- getSolver abstractionCover
    writeLog 2 "instantiate" $ text "Current abstract types:" <+> text (show cover)
    -- back propagation of the error types to get all split information
    propagate env prog $ compactAbstractType at
    -- get the split pairs
    splits <- getSolver splitTypes
    let tvs = env ^. boundTypeVars
    let sortedSplits = sortBy (flip (compareAbstract tvs)) (Set.toList splits)
    mapM_ (\t -> overSolver typeToIndex (\m -> Map.insert t (Map.size m) m)) sortedSplits
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
    t2id <- getSolver typeToIndex
    let splitIdx = map (t2id Map.!) (Set.toList splits)
    cloneNames <- if noClone then return []
                             else mapM addCloneFunction splitIdx
    -- update the higer order query arguments
    let hoArgs = Map.filter isFunctionType (env ^. arguments)
    mapM_ addMusters (Map.keys hoArgs)
    -- call the refined encoder on these signatures and disabled signatures
    t2id <- getSolver typeToIndex
    writeLog 2 "refineSemantic" $ pretty sortedSplits
    writeLog 2 "refineSemantic" $ pretty splits
    writeLog 2 "refineSemantic" $ pretty t2id
    return SplitInfo { newPlaces = map ((Map.!) t2id) (Set.toList splits)
                     , removedTrans = removables
                     , newTrans = addedSigs ++ cloneNames
                     }
  where
    splitTransitions at = do
        setSolver toRemove []
        cover <- getSolver abstractionCover
        t2tr <- getSolver type2transition
        gm <- getSolver groupMap
        t2id <- getSolver typeToIndex
        gn <- getSolver nameToGroup
        let parents = map (t2id Map.!) $ HashMap.keys $ HashMap.filter (Set.member at) cover
        let gids = Set.unions $ map (\p -> HashMap.lookupDefault Set.empty p t2tr) parents
        let pids = Set.unions $ map (\g -> Map.findWithDefault Set.empty g gm) (Set.toList gids)
        writeLog 2 "splitTransitions" $ pretty pids
        sigs <- mapM (splitTransition env at) (Set.toList pids)
        let hoArgs = Map.filter isFunctionType (env ^. arguments)
        let tvs = env ^. boundTypeVars
        let envSigs = Map.fromList (concat sigs)
        let allSigs = concat sigs
        sigs' <- mkGroups env $ Map.fromList allSigs
        mapM_ addEncodedFunction (Map.toList sigs')

        -- update the group information by the current toRemove
        toCoalesce <- getExperiment coalesceTypes
        removables <- changeGroups toCoalesce
        writeLog 3 "splitTransitions (toAdd, removables)" $ pretty removables

        let adds = Map.toList sigs'
        overSolver mustFirers (HashMap.map (filter (`notElem` removables)))
        overSolver nameMapping (`Map.withoutKeys` Set.fromList removables)
        return (adds, removables)

    changeGroups True = do
        removables' <- getSolver toRemove
        writeLog 3 "changeGroups" $ pretty removables'
        splitGroups removables'
    changeGroups False = getSolver toRemove

    splitGroups :: MonadIO m => [Id] -> PNSolver m [Id]
    splitGroups removables = do
        -- Step 1: modify groupmap to exclude all those in removables
        -- Some groups may no longer exist as a result of this operation.
        -- Some groups representative may not longer be valid.
        -- TODO: This operation could be slow. find a way to go from id -> groupId (or id -> abstrTy)
        gm <- getSolver groupMap
        let shrinkedGm = map (\(k, vs) -> (k, shrinkSet (Set.fromList removables) vs)) $ Map.toList gm
        let (gm', removed) = partition (isJust . snd) shrinkedGm
        setSolver groupMap (Map.fromList $ map (\(a, b) -> (a, fromJust b)) gm')
        let removeSet = Set.fromList removables
        overSolver nameMapping (`Map.withoutKeys` removeSet)
        overSolver currentSigs (`Map.withoutKeys` removeSet)
        overSolver nameToGroup (`Map.withoutKeys` removeSet)
        return (map fst removed)

    shrinkSet :: Set Id -> Set Id -> Maybe (Set Id)
    shrinkSet toRemove ids = let
        ids' = Set.difference ids toRemove
        in if Set.null ids' then Nothing else Just ids'


initNet :: MonadIO m => Environment -> RType -> PNSolver m PetriNet
initNet env dst = withTime ConstructionTime $ do
    -- reset the solver state
    setSolver functionMap HashMap.empty
    setSolver currentSigs Map.empty
    setSolver type2transition HashMap.empty
    setSolver instanceMapping HashMap.empty
    -- add all/selected symbols into the net
    names <- if selection then selectComp env >> getSolver (explorer . selectedNames)
                          else return $ Map.keysSet (env ^. symbols)
    let mySymbols = Map.restrictKeys (env ^. symbols) names
    sigs <- addSignatures env mySymbols
    let allTy = nub $ concatMap (\(FunctionCode _ _ args res) -> args ++ res) (Map.elems sigs)
    -- add clone functions for each type
    noClone <- getExperiment disableCopy
    unless noClone $ mapM_ addCloneFunction allTy
    -- add higher order query arguments
    let hoArgs = Map.filter isFunctionType (env ^. arguments)
    mapM_ addMusters (Map.keys hoArgs)
    -- prepare the petri net
    t2id <- getSolver typeToIndex
    fm <- getSolver functionMap
    let maxIndex = Map.size t2id
    updateSrcTgt env dst
    return $ mkPetriNet maxIndex (HashMap.elems fm)
  where
    abstractSymbol id sch = do
        t <- freshType sch
        let absTy = toAbstractType (shape t)
        return (id, absTy)

refineNet :: MonadIO m => PetriNet -> SplitInfo -> PNSolver m PetriNet
refineNet net (SplitInfo _ removes adds) = do
    t2id <- getSolver typeToIndex
    fm <- getSolver functionMap
    let fc = map (fm HashMap.!) adds
    let maxIndex = Map.size t2id
    writeLog 2 "refineNet transitions" $ text $ show (transitions net)
    let rmNet = foldr rmTransition net removes
    writeLog 2 "refineNet removes" $ pretty removes
    let resNet = foldr (addTransition maxIndex) rmNet fc
    nameMap <- getSolver nameMapping
    writeLog 2 "refineNet nameMap" $ pretty nameMap
    st <- gets $ view _2
    src <- getSolver sourceTypes
    dst <- getSolver targetType
    
    let st' = clearPath removes st
    modify $ set _2 st'
    return resNet

addEncodedFunction :: MonadIO m => (Id, FunctionCode) -> PNSolver m ()
addEncodedFunction (id, ef) = do
    overSolver functionMap (HashMap.insert id ef)
    updateTy2Tr ef

findPath :: MonadIO m => Environment -> PetriNet -> PNSolver m [Id]
findPath env net = withTime SolverTime $ do
    trials <- getSolver triedPaths
    loc <- getSolver currentLoc
    maxDepth <- getExperiment maxApplicationDepth
    currMaxDepth <- getSolver (explorer . currentDepth)
    writeLog 0 "findPath" $ text "current depth" <+> pretty loc
    st <- gets (view _2)
    let (res, st') = bisearchPN net (searchDepth st) currMaxDepth trials (forwards st) (backwards st)
    modify $ set _2 st'
    case res of
        [] | loc >= maxDepth -> do
                mesgChan <- getSolver messageChan
                liftIO $ writeChan mesgChan (MesgClose CSNoSolution)
                error "cannot find a path"
            | loc >= currMaxDepth && selection -> do
                addOrNot <- addMoreComponent
                case addOrNot of
                    Right newNames -> do
                        writeLog 1 "findPath" $ text "adding functions:" <+> pretty newNames
                        net' <- refineNet net (SplitInfo [] [] newNames)
                        findPath env net'
                    Left _ -> do
                        setSolver (explorer . currentDepth) maxDepth
                        overSolver currentLoc (+ 1)
                        findPath env net
            | otherwise -> error "cannot find a path in the given depth"
        _  -> return res
    where
        addMoreComponent = do
            oldNames <- getSolver (explorer . selectedNames)
            writeLog 1 "addMoreComponents" $ pretty oldNames
            selectComp env
            names <- getSolver (explorer . selectedNames)
            if oldNames == names
                then return (Left names)
                else do
                    let newNames = names `Set.difference` oldNames
                    let mySymbols = Map.restrictKeys (env ^. symbols) newNames
                    sigs <- addSignatures env mySymbols
                    let hoArgs = Map.filter isFunctionType (env ^. arguments)
                    mapM_ addMusters (Map.keys hoArgs)
                    return $ Right (Map.keys sigs)

findProgram :: MonadIO m
            => Environment -- the search environment
            -> RType       -- the goal type
            -> PetriNet
            -> PNSolver m ()
findProgram env dst net = do
        setSolver splitTypes Set.empty
        args <- getSolver sourceTypes
        target <- getSolver targetType
        nameMap <- getSolver nameMapping
        writeLog 2 "findProgram" $ text "calling findProgram"
        path <- findPath env net
        writeLog 2 "findProgram" $ text "unfiltered path:" <+> pretty path
        let usefulTrans = filter (\tr -> skipClone tr && skipEmpty tr) path
        withTime FormerTime (enumeratePath usefulTrans) >>= handleResult path
    where
        handleResult path res =
            case res of
                NotFound -> overSolver triedPaths (Set.insert path) >> mzero
                Found soln -> do
                    writeSolution soln
                    overSolver currentSolutions ((:) soln)
                    overSolver triedPaths (Set.insert path)
                MoreRefine err -> ifte mzero (const mzero) (nextSolution err)

        checkPath gpSig p = do
            disrel <- getExperiment disableRelevancy
            nameMap <- getSolver nameMapping
            let hoArgs =
                    Map.keys $
                    Map.filter isFunctionType (env ^. arguments)
            let hoArgs' = map (++ hoPostfix) hoArgs ++ hoArgs
            let hoAlias = Map.keysSet $ Map.filter (`elem` hoArgs') nameMap
            let filterPaths p =
                                let p' =
                                        map
                                            (\x ->
                                                 replaceId hoPostfix "" $
                                                 lookupWithError "nameMapping" x nameMap)
                                            p
                                 in disrel || all (`elem` p') hoArgs
            guard (filterPaths p)
            codeResult <- fillSketch gpSig p
            writeLog 1 "checkUntilFail" $ pretty codeResult
            checkResult <- withTime TypeCheckTime $
                            firstCheckedOrError $
                            sortOn (Data.Ord.Down . length) $
                            Set.toList codeResult
            rs <- getExperiment refineStrategy
            stop <- getExperiment stopRefine
            placeNum <- getExperiment threshold
            cover <- getSolver abstractionCover
            case checkResult of
                Nothing -> mzero
                Just (Left code) -> do
                    mbSln <- checkSolution code
                    case mbSln of
                        Nothing -> mzero
                        Just p  -> return (Found p)
                Just (Right err)
                    | not (doRefine rs) || (stop && coverSize cover >= placeNum) -> do
                        cover <- getSolver abstractionCover
                        fm <- getSolver functionMap
                        overSolver solverStats (\s -> s {
                              numOfPlaces = Map.insert (iterations s + 1) (coverSize cover) (numOfPlaces s)
                            , numOfTransitions = Map.insert (iterations s + 1) (HashMap.size fm) (numOfTransitions s)
                            })
                        mzero
                    | otherwise -> return (MoreRefine err)

        enumeratePath path = do
            gm <- getSolver groupMap
            uc <- getSolver useCount
            nameMap <- getSolver nameMapping
            fm <- getSolver functionMap
            let getFuncs p = Map.findWithDefault Set.empty p gm
            let sortFuncs p =
                    sortOn
                        (\x ->
                             let name = lookupWithError "nameMapping" x nameMap
                              in Map.findWithDefault 0 name uc) $
                    Set.toList p
            let allPaths = map (sortFuncs . getFuncs) path
            let gpSig = map (findFunction fm) path
            msum $ map (checkPath gpSig) (sequence allPaths)

        skipClone = not . isInfixOf "|clone"
        skipEmpty = (/=) ""

        generateCode initialFormer src args sigs = do
            tgt <- getSolver targetType
            cover <- getSolver abstractionCover
            disrel <- getExperiment disableRelevancy
            t2id <- getSolver typeToIndex
            let bound = env ^. boundTypeVars
            let rets = map (t2id Map.!) $ filter (isSubtypeOf bound tgt) (allTypesOf cover)
            writeLog 1 "generateCode" $ pretty src
            writeLog 1 "generateCode" $ pretty rets
            liftIO (evalStateT (generateProgram sigs src args rets disrel) initialFormer)

        removeSuffix = removeLast '|'

        substName [] []           = []
        substName (n:ns) (fc:fcs) = fc { funName = n } : substName ns fcs

        findFunction fm name = fromMaybe (error $ "cannot find function name " ++ name)
                                         (HashMap.lookup name fm)

        fillSketch gpSig firedTrans = do
            src <- getSolver sourceTypes
            nameMap <- getSolver nameMapping
            t2id <- getSolver typeToIndex
            let args = Map.keys $ foArgsOf env
            writeLog 1 "fillSketch" $ text "found path" <+> pretty firedTrans
            mapM_ (\f -> do
                let name = lookupWithError "nameMapping" f nameMap
                overSolver useCount $ Map.insertWith (+) name 1) firedTrans
            let sigs = substName firedTrans gpSig
            writeLog 1 "fillSketch" $ text "found filtered sigs" <+> pretty sigs
            let initialFormer = FormerState HashMap.empty []
            let src' = map ((Map.!) t2id) src
            withTime FormerTime $ generateCode initialFormer src' args sigs

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
                           ParseOk exp         -> toSynquidProgram exp
                           ParseFailed loc err -> error err
            mapping <- getSolver nameMapping
            counter <- getSolver nameCounter
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
            setSolver nameCounter (checkerNameCounter checkerState')
            if isChecked checkerState'
                then return (Left btm)
                else do
                    let tyBtm' = toAbstractType $ typeSubstitute tass tyBtm
                    let absDst = toAbstractType dst
                    absBtm <- observeT $ pickGeneralization tyBtm' absDst
                    -- writeLog 1 "parseAndCheck" $ text "get distinguished type" <+> pretty absBtm
                    return (Right (btm, absBtm))

        nextSolution (prog, at) = do
            cover <- getSolver abstractionCover
            splitInfo <- withTime RefinementTime (refineSemantic env prog at)
            writeLog 1 "nextSolution" $ text "get split info" <+> pretty splitInfo
            -- add new places and transitions into the petri net
            cover <- getSolver abstractionCover
            funcs <- getSolver functionMap
            overSolver solverStats (\s ->
                s   { iterations = iterations s + 1
                    , numOfPlaces = Map.insert (iterations s) (coverSize cover) (numOfPlaces s)
                    , numOfTransitions = Map.insert (iterations s) (HashMap.size funcs) (numOfTransitions s)
                    })
            net' <- refineNet net splitInfo
            updateSrcTgt env dst
            findProgram env dst net'

        checkSolution code = do
            solutions <- getSolver currentSolutions
            mapping <- getSolver nameMapping
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

runPNSolver :: MonadIO m => Environment -> RType -> PNSolver m ()
runPNSolver env t = do
    net <- withTime TotalSearch $ initNet env t
    writeLog 2 "runPNSolver" $ text (show (transitions net))
    cnt <- getExperiment solutionCnt
    t2id <- getSolver typeToIndex
    depth <- getExperiment maxApplicationDepth
    let args = map toFunDts (Map.elems (env ^. arguments))
    let foArgs = Map.filter (not . isFunctionType) (env ^. arguments)
    let nonArgSyms = Map.difference (env ^. symbols) foArgs
    let rets = [t]
    setSolver (explorer . forwardSet) (Set.fromList args)
    setSolver (explorer . backwardSet) (Set.fromList rets)
    setSolver (explorer . nullaries) (Map.filter ((==) 0 . arity . toMonotype) nonArgSyms)
    unless selection $ setSolver (explorer . currentDepth) depth
    findProgram env t net
    msgChan <- getSolver messageChan
    liftIO $ writeChan msgChan (MesgClose CSNormal)

writeSolution :: MonadIO m => UProgram -> PNSolver m ()
writeSolution code = do
    stats <- getSolver solverStats
    loc <- getSolver currentLoc
    msgChan <- getSolver messageChan
    let stats' = stats {pathLength = loc}
    liftIO $ writeChan msgChan (MesgP (code, stats'))
    liftIO $ printSolution code
    liftIO $ print stats'

{- helper functions -}
addCloneFunction :: MonadIO m => Int -> PNSolver m Id
addCloneFunction ty = do
    let fname = show ty ++ "|clone"
    let fc = FunctionCode fname [] [ty] [ty, ty]
    overSolver functionMap (HashMap.insert fname fc)
    updateTy2Tr fc
    return fname

doRefine :: RefineStrategy -> Bool
doRefine NoGar      = False
doRefine NoGar0     = False
doRefine SypetClone = False
doRefine _          = True

updateTy2Tr :: MonadIO m => FunctionCode -> PNSolver m ()
updateTy2Tr (FunctionCode f _ args res) = do
    let addTransition k tid = HashMap.insertWith Set.union k (Set.singleton tid)
    let includedTyps = nub (args ++ res)
    mapM_ (\t -> overSolver type2transition (addTransition t f)) includedTyps

updateSrcTgt :: MonadIO m
            => Environment
            -> RType
            -> PNSolver m ([AbstractSkeleton], AbstractSkeleton)
updateSrcTgt env dst = do
    -- reset source and destination types
    let binds = env ^. boundTypeVars
    abstraction <- getSolver abstractionCover
    tgt <- currentAbst binds abstraction (toAbstractType (shape dst))
    setSolver targetType tgt

    let foArgs = Map.filter (not . isFunctionType) (env ^. arguments)
    srcTypes <- mapM ( currentAbst binds abstraction
                     . toAbstractType) $ Map.elems foArgs
    setSolver sourceTypes srcTypes
    noClone <- getExperiment disableCopy
    t2id <- getSolver typeToIndex
    let argIdxs = map (t2id Map.!) (tgt:srcTypes)
    unless noClone $ mapM_ addCloneFunction argIdxs
    return (srcTypes, tgt)

type EncoderArgs = (Int
                    , HashMap Id [Id]
                    , [AbstractSkeleton]
                    , [FunctionCode]
                    , HashMap Int (Set Id))

prepEncoderArgs :: MonadIO m
                => Environment
                -> AbstractSkeleton
                -> PNSolver m EncoderArgs
prepEncoderArgs env tgt = do
    cover <- getSolver abstractionCover
    loc <- getSolver currentLoc
    funcs <- getSolver functionMap
    t2tr <- getSolver type2transition
    musters <- getSolver mustFirers
    let bound = env ^. boundTypeVars
    let accepts = superTypeOf bound cover tgt
    let rets = sortBy (compareAbstract bound) accepts
    let sigs = HashMap.elems funcs
    writeLog 3 "prepEncoderArgs" $ text "current must firers" <+> pretty (HashMap.toList musters)
    return (loc, musters, rets, sigs, t2tr)

foArgsOf :: Environment -> Map Id RType
foArgsOf = Map.filter (not . isFunctionType) . _arguments

noneInst instMap id t = not (HashMap.member (id, sort $ absFunArgs id t) instMap)

diffInst tvs instMap id t = let oldt = snd (fromJust $ HashMap.lookup (id, sort $ absFunArgs id t) instMap)
                             in lastAbstract t /= lastAbstract oldt && isSubtypeOf tvs (lastAbstract t) (lastAbstract oldt)

excludeUseless :: MonadIO m => Id -> AbstractSkeleton -> PNSolver m ()
excludeUseless id ty = do
    instMap <- getSolver instanceMapping
    let (tid, ty') = fromJust (HashMap.lookup (id, sort $ absFunArgs id ty) instMap)
    writeLog 3 "excludeUseless" $ text "delete" <+> pretty tid <+> text "==>" <+> pretty id <+> text "::" <+> pretty ty'
    overSolver toRemove ((:) tid)

assemblePair :: AbstractSkeleton
            -> AbstractSkeleton
            -> AbstractSkeleton
assemblePair first secod | absFunArgs "fst" first == absFunArgs "snd" secod =
    let AFunctionT p f = first
        AFunctionT _ s = secod
     in AFunctionT p (AFunctionT f s)
assemblePair first second = error "fst and snd have different arguments"
