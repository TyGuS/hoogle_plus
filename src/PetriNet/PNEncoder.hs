{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PetriNet.PNEncoder(
     encoderInit
    , encoderSolve
    , encoderRefine
    ) where

import Data.Maybe
import Data.List
import Data.List.Extra
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Z3.Monad hiding(Z3Env, newEnv)
import qualified Z3.Base as Z3
import Control.Monad.State
import System.CPUTime
import Text.Printf
import Data.Text (pack, unpack, replace)

import Types.Common
import Types.Encoder
import Types.Abstract
import Synquid.Util

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . z3env)
    getContext = gets (envContext . z3env)
    getOptimize = gets (envOptimize . z3env)

-- | create a new encoder in z3
createEncoder :: [Id] -> Id -> [FunctionCode] -> Encoder ()
createEncoder inputs ret sigs = do
    places <- gets (Map.keys . ty2tr)
    transIds <- gets (nubOrd . concat . Map.elems . ty2tr)
    -- create all the type variables for encoding
    createVariables places transIds
    transitions <- mapM foldFunc sigs
    -- add all the constraints for the solver
    createConstraints places transitions
    -- set initial and final state for solver
    setInitialState inputs places
    setFinalState ret places

-- | set the initial state for the solver, where we have tokens only in void or inputs
-- the tokens in the other places should be zero
setInitialState :: [Id] -> [Id] -> Encoder ()
setInitialState inputs places = do
    let nonInputs = filter (\k -> notElem k inputs && not (head k == '-')) places
    let inputCounts = map (\t -> (head t, length t)) (group (sort inputs))
    let typeCounts = inputCounts ++ (map (\t -> (t, 0)) nonInputs)
    -- assign tokens to each types
    mapM_ (uncurry assignToken) typeCounts
  where
    assignToken p v = do
        placeMap <- place2variable <$> get
        tVar <- mkZ3IntVar $ findVariable (p, 0) placeMap
        if ('-' : p) `elem` places
           then do
                negVar <- mkZ3IntVar $ findVariable ('-' : p, 0) placeMap
                initial <- mkIntNum v
                mkAdd [negVar, initial] >>= mkEq tVar >>= assert
           else mkIntNum v >>= mkEq tVar >>= assert

-- | set the final solver state, we allow only one token in the return type
-- and maybe several tokens in the "void" place
setFinalState :: Id -> [Id] -> Encoder ()
setFinalState ret places = do
    -- the return value should have only one token
    includeRet
    -- other places excluding void and ret should have nothing
    let nonOutputs = filter ((/=) ret) places
    mapM_ excludeOther nonOutputs
  where
    includeRet = do
        placeMap <- place2variable <$> get
        l <- loc <$> get
        retVar <- mkZ3IntVar $ findVariable (ret, l) placeMap
        mkIntNum 1 >>= mkEq retVar >>= assert
    excludeOther p = do
        l <- loc <$> get
        placeMap <- place2variable <$> get
        when (p /= "void") $ do
            tVar <- mkZ3IntVar $ findVariable (p, l) placeMap
            mkIntNum 0 >>= mkEq tVar >>= assert

solveAndGetModel :: Encoder [(Id, Int)]
solveAndGetModel = do
    prev <- prevChecked <$> get
    when (prev) (do
        toBlock <- block <$> get
        assert toBlock)
    -- res <- optimizeCheck
    res <- check
    l <- loc <$> get
        {- when (l == 1) (do
        solverStr <- solverToString
        liftIO $ putStrLn solverStr
        transMap <- transition2id <$> get
        liftIO $ print transMap
        placeMap <- place2variable <$> get
        liftIO $ print placeMap
        transMap' <- id2transition <$> get
        liftIO $ print transMap'
                  ) -}
    case res of
        Sat -> do
            model <- solverGetModel
            modelStr <- modelToString model
            selected <- mapM (checkLit model) [0..(l-1)]
            blockTrs <- mapM (uncurry blockTr) (zip [0..(l-1)] selected)
            -- placeMap <- place2variable <$> get
            -- blockP <- mapM (checkIntLit model) $ HashMap.toList placeMap
            blockAss <- mkAnd blockTrs >>= mkNot
            st <- get
            put $ st { block = blockAss }
            selectedNames <- getTrNames selected
            return (zip selectedNames [0,1..])
        Unsat -> do
            liftIO $ print "unsat"
            return []
        Undef -> do
            liftIO $ print "undef"
            return []
  where
    getTrNames selected = do
        lv <- abstractionLv <$> get
        transMap <- id2transition <$> get
        let transNames = map (\id -> findVariable (fromIntegral id) transMap) selected
        let selectedTrs = map fst (filter ((==) lv . snd) transNames)
        return selectedTrs

    checkLit model t = do
        lv <- abstractionLv <$> get
        tsMap <- time2variable <$> get
        tsVar <- mkZ3IntVar (findVariable (t, lv) tsMap)
        bMay <- evalInt model tsVar
        case bMay of
            Just b -> return b
            Nothing -> error $ "cannot eval the variable" ++ show (t, lv)

    blockTr t tr = do
        lv <- abstractionLv <$> get
        tsMap <- time2variable <$> get
        tsVar <- mkZ3IntVar (findVariable (t, lv) tsMap)
        mkIntNum tr >>= mkEq tsVar

    checkIntLit model (k, v) = do
        pVar <- mkZ3IntVar v
        iMay <- eval model pVar
        case iMay of
            Just i -> mkEq pVar i
            Nothing -> error $ "cannot eval the variable" ++ show k

encoderInit :: Int -> [Id] -> [Id] -> Id -> [FunctionCode] -> Map Id [Id] -> IO EncodeState
encoderInit loc hoArgs inputs ret sigs t2tr = do
    z3Env <- initialZ3Env
    false <- Z3.mkFalse (envContext z3Env)
    let initialState = EncodeState z3Env false loc 0 0 1 HashMap.empty HashMap.empty HashMap.empty HashMap.empty HashMap.empty hoArgs [] t2tr False
    execStateT (createEncoder inputs ret sigs) initialState

encoderSolve :: EncodeState -> IO ([(Id, Int)], EncodeState)
encoderSolve st = runStateT solveAndGetModel st

-- | wrap some action with time measuring and print out the execution time
withTime :: String -> Encoder a -> Encoder a
withTime desc f = do
    start <- liftIO getCPUTime
    res <- f
    end <- liftIO getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    let time = printf "%s time: %0.3f sec\n" desc (diff :: Double)
    liftIO $ putStrLn time
    return res

    -- f

encoderRefine :: SplitInfo -> [FunctionCode] -> Map Id [Id] -> [Id] -> Id -> Encoder ()
encoderRefine info sigs t2tr inputs ret = do
    {- update the abstraction level -}
    st <- get
    put $ st { abstractionLv = abstractionLv st + 1 
             , ty2tr = t2tr
             }

    {- operation on places -}
    let newPlaces = map show (concat (snd (unzip (splitedPlaces info))))
    let typeClones = map (\(p, ps) -> (show p ++ "|clone", map (\n -> show n ++ "|clone") ps)) (splitedPlaces info)

    {- operation on transitions -}
    let transIds = concat (snd (unzip (splitedGroup info)))
    let allTransIds = fst (unzip (splitedGroup info)) ++ transIds ++ (fst (unzip typeClones)) ++ (concat (snd (unzip typeClones)))
    -- some of the transitions are splitted
    let existingTrans = findVariable (abstractionLv st) (transition2id st)
    let newTransId = concat (snd (unzip typeClones)) ++ transIds
    -- other transition have no change but need to be copied to the next level
    let noSplit k _ = not (elem k allTransIds)
    let oldTrans = HashMap.keys (HashMap.filterWithKey noSplit existingTrans)
    let splitTrans = typeClones ++ splitedGroup info

    l <- loc <$> get

    -- add new place, transition and timestamp variables
    mapM_ addPlaceVar newPlaces
    addTransitionVar (newTransId ++ oldTrans)
    mapM_ addTimestampVar [0..(l-1)]

    newTrans <- mapM foldFunc sigs
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- newTrans ]

    -- refine the sequential constraints
    -- but enable transitions between abstraction levels fired simultaneously
    withTime "sequentialTransitions" sequentialTransitions

    -- refine the precondition constraints
    withTime "preconditions" $ mapM_ (uncurry preconditionsTransitions) allTrans

    -- refine the postcondition constraints
    withTime "postconditions" $ mapM_ (uncurry postconditionsTransitions) allTrans

    -- refine the no transition constraints
    let posMerge = map (\(p, ps) -> (show p, map show ps)) (splitedPlaces info)
    -- for support of hof, we need to merge some negative places if exists
    let negPlaceIds = map (drop 1) (filter ((==) '-' . head) newPlaces)
    let negMergeIds = filter (not . null . snd) (map (\(p, ps) -> (p, filter (`elem` negPlaceIds) ps)) posMerge)
    let negMerge = map (\(p, ps) -> ('-':p, map ('-':) ps)) negMergeIds
    let toMerge = posMerge ++ negMerge
    withTime "placeMerge" $ mapM_ (\t -> mapM_ (uncurry (placeMerge t)) toMerge) [0..l]
    withTime "transMerge" $ mapM_ (uncurry transMerge) [ (tr, t) | tr <- splitTrans, t <- [0..(l-1)] ]
    withTime "transKeep" $ mapM_ (uncurry transClone) [ (tr, t) | tr <- oldTrans, t <- [0..(l-1)] ]
    withTime "noTransitionTokens" $ mapM_ (uncurry noTransitionTokens) [(t, p) | p <- newPlaces, t <- [0..(l-1)]]

    -- refine the must firers
    withTime "mustFireTransitions" mustFireTransitions

    -- set new initial and final state
    setInitialState inputs newPlaces
    setFinalState ret newPlaces
  where

    placeMerge t p nps = do
        placeMap <- place2variable <$> get
        let npVars = map (\np -> findVariable (np, t) placeMap) nps
        npZ3Vars <- mapM mkZ3IntVar npVars
        let pVar = findVariable (p, t) placeMap
        pZ3Var <- mkZ3IntVar pVar
        mkAdd npZ3Vars >>= mkEq pZ3Var >>= assert

    transMerge (tr, ntrs) t = do
        transMap <- transition2id <$> get
        tsMap <- time2variable <$> get
        lv <- abstractionLv <$> get
        let ntrIds = map (\ntr -> findVariable ntr (findVariable lv transMap)) ntrs
        let trId = findVariable tr (findVariable (lv - 1) transMap)
        let tsVar = findVariable (t, lv) tsMap
        let lastTsVar = findVariable (t, lv - 1) tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        lastTsZ3Var <- mkZ3IntVar lastTsVar
        selectTr <- mkIntNum trId >>= mkEq lastTsZ3Var
        selectNtrs <- mapM mkIntNum ntrIds >>= mapM (mkEq tsZ3Var)
        mkOr selectNtrs >>= mkIff selectTr >>= assert

    transClone tr t = do
        transMap <- transition2id <$> get
        tsMap <- time2variable <$> get
        lv <- abstractionLv <$> get
        trVar <- mkIntNum (findVariable tr (findVariable lv transMap))
        lastTrVar <- mkIntNum (findVariable tr (findVariable (lv - 1) transMap))
        tsVar <- mkZ3IntVar (findVariable (t, lv) tsMap)
        lastTsVar <- mkZ3IntVar (findVariable (t, lv - 1) tsMap)
        selectLastTr <- mkEq lastTsVar lastTrVar
        selectTr <- mkEq tsVar trVar
        mkIff selectLastTr selectTr >>= assert

addPlaceVar ::  Id -> Encoder ()
addPlaceVar p = do
    l <- loc <$> get
    mapM_ addPlaceVarAt [0..l]
  where
    addPlaceVarAt t = do
        st <- get
        let placeVar = Variable (variableNb st) (p ++ "_" ++ show t) t 0 VarPlace
        when (not (HashMap.member (p, t) (place2variable st)))
             (put $ st { place2variable = HashMap.insert (p, t)
                                                         placeVar
                                                         (place2variable st)
                       , variableNb = variableNb st + 1
                       })

-- | add transition mapping from (tr, lv) to integer id
addTransitionVar :: [Id] -> Encoder ()
addTransitionVar trs = do
    lv <- gets abstractionLv
    start <- gets transitionNb -- inclusive start
    mapM_ addTransitionVarFor trs
    end <- gets transitionNb -- exclusive end
    st <- get
    put $ st { lv2range = HashMap.insert lv (start, end) (lv2range st) }
  where
    addTransitionVarFor tr = do
        lv <- abstractionLv <$> get
        st <- get
        let tid = transitionNb st
        -- liftIO $ putStrLn $ show tid ++ ": " ++ show (transitionId tr)
        when (not (HashMap.member tr (HashMap.lookupDefault HashMap.empty lv (transition2id st))))
             (put $ st { transitionNb = 1 + transitionNb st
                       , transition2id = HashMap.insertWith HashMap.union lv (HashMap.singleton tr tid) (transition2id st)
                       , id2transition = HashMap.insert tid (tr, lv) (id2transition st)
                       })

addTimestampVar :: Int -> Encoder ()
addTimestampVar t = do
    absLv <- gets abstractionLv
    mapM_ addTimestampVarAt [0..absLv]
  where
    addTimestampVarAt l = do
        st <- get
        let tsVar = Variable (variableNb st) ("ts_" ++ show t ++ "_" ++ show l) t l VarTimestamp
        when (not (HashMap.member (t, l) (time2variable st)))
             (put $ st { time2variable = HashMap.insert (t, l) tsVar (time2variable st)
                       , variableNb = variableNb st + 1
                       })

-- | map each place and transition to a variable in z3
createVariables :: [Id] -> [Id] -> Encoder ()
createVariables places transitions = do
    l <- gets loc
    -- add place variables
    mapM_ addPlaceVar places
    -- add transition mapping
    addTransitionVar transitions
    -- add timestamp variables
    mapM_ addTimestampVar [0..(l-1)]

createConstraints :: [Id] -> [FoldedFunction] -> Encoder ()
createConstraints places transitions = do
    -- prepare constraint parameters
    l <- gets loc
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- transitions]
    let allPlaces = [(t, p) | t <- [0..(l-1)], p <- places]

    -- we assume we only have the first abstraction level each time when we call this method
    withTime "sequantial transitions:" sequentialTransitions
    -- we do not have parallel transitions in the first time encoding

    withTime "preconditions:" (mapM_ (uncurry preconditionsTransitions) allTrans)

    withTime "postconditions:" (mapM_ (uncurry postconditionsTransitions) allTrans)

    withTime "no fire:" (mapM_ (uncurry noTransitionTokens) allPlaces)

    withTime "must fire:" mustFireTransitions

mkZ3BoolVar ::  Variable -> Encoder AST
mkZ3BoolVar var = do
    varSymbol <- mkIntSymbol (varId var)
    boolS <- mkBoolSort
    const <- mkConst varSymbol boolS
    return const

mkZ3IntVar :: Variable -> Encoder AST
mkZ3IntVar var = do
    varSymbol <- mkIntSymbol (varId var)
    intS <- mkIntSort
    const <- mkConst varSymbol intS
    return const

findVariable :: (Eq k, Hashable k, Show k) => k -> HashMap k v -> v
findVariable k m = case HashMap.lookup k m of
                        Just v -> v
                        Nothing -> error $ "cannot find variable for " ++ show k

-- | at each timestamp, only one transition can be fired, we restrict the
-- fired transition id range here
sequentialTransitions :: Encoder ()
sequentialTransitions = do
    l <- gets loc
    lv <- gets abstractionLv
    lvRange <- gets lv2range
    let rng = findVariable lv lvRange
    mapM_ (fireAt rng) [0..(l-1)]
  where
    fireAt rng t = do
        tsMap <- time2variable <$> get
        lv <- abstractionLv <$> get
        let tsVar = findVariable (t, lv) tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        start <- mkIntNum (fst rng)
        mkGe tsZ3Var start >>= assert
        end <- mkIntNum (snd rng)
        mkLt tsZ3Var end >>= assert

-- | if this place has no connected transition fired,
-- it has the same # of tokens
noTransitionTokens :: Int -> Id -> Encoder ()
noTransitionTokens t p = do
    lv <- gets abstractionLv
    trans <- gets (findVariable lv . transition2id)
    t2tr <- gets ty2tr
    let transitions = map (\i -> findVariable i trans) (Map.findWithDefault [] p t2tr)
    noFireLvs <- noFireAt transitions p t
    noFire <- mkOr noFireLvs >>= mkNot
    placeMap <- place2variable <$> get
    curr <- mkZ3IntVar $ findVariable (p, t) placeMap
    next <- mkZ3IntVar $ findVariable (p, t + 1) placeMap
    mkEq curr next >>= mkImplies noFire >>= assert
  where
    noFireAt transitions p t = do
        idVars <- mapM mkIntNum transitions
        tsMap <- time2variable <$> get
        lv <- abstractionLv <$> get
        let tsVar = findVariable (t, lv) tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        mapM (mkEq tsZ3Var) idVars

-- | calculate the preconditions for each transition to be fired
preconditionsTransitions :: Int -> FoldedFunction -> Encoder ()
preconditionsTransitions t (FoldedFunction tid pcnt _) = fireFor
  where
    -- get enough resources for current places to fire the tr
    getSatisfiedPlaces trVar (p, cnt) = do
        w <- mkIntNum cnt
        placeMap <- gets place2variable
        pVar <- mkZ3IntVar (findVariable (p, t) placeMap)
        mkGe pVar w >>= mkImplies trVar >>= assert

    fireFor = do
        transMap <- gets transition2id
        tsMap <- gets time2variable
        lv <- gets abstractionLv
        let tsVar = findVariable (t, lv) tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        trVar <- mkIntNum tid >>= mkEq tsZ3Var
        mapM_ (getSatisfiedPlaces trVar) pcnt

postconditionsTransitions :: Int -> FoldedFunction -> Encoder ()
postconditionsTransitions t (FoldedFunction tid _ rcnt) = do
    transMap <- transition2id <$> get
    tsMap <- time2variable <$> get
    lv <- abstractionLv <$> get
    let tsVar = findVariable (t, lv) tsMap
    tsZ3Var <- mkZ3IntVar tsVar
    trVar <- mkIntNum tid >>= mkEq tsZ3Var
    mapM_ (mkChange t trVar) rcnt
  where
    mkChange t trVar (p, diff) = do
        let d = if p == "void" then 0 else -diff
        placeMap <- place2variable <$> get
        before <- mkZ3IntVar $ findVariable (p, t) placeMap
        after <- mkZ3IntVar $ findVariable (p, t + 1) placeMap
        diffw <- mkIntNum d
        mkAdd [before, diffw] >>= mkEq after >>= mkImplies trVar >>= assert

mustFireTransitions ::  Encoder ()
mustFireTransitions = do
    must <- mustFirers <$> get
    lv <- abstractionLv <$> get
    transitions <- findVariable lv . transition2id <$> get
    let mustTrans = HashMap.filterWithKey (\k _ -> nameInMust must k) transitions
    mapM_ fireTransition mustTrans
  where
    nameInMust must name = foldr ((||) . flip isInfixOf name) False must
    fireTransition tid = do
        l <- loc <$> get
        lv <- abstractionLv <$> get
        tsMap <- time2variable <$> get
        trId <- mkIntNum tid
        tsVars <- mapM (\t -> mkZ3IntVar (findVariable (t, lv) tsMap)) [0..(l-1)]
        trVars <- mapM (mkEq trId) tsVars
        mkOr trVars >>= assert

-- helper function
-- group parameters in function codes
foldFunc :: FunctionCode -> Encoder FoldedFunction
foldFunc (FunctionCode name hoParams params rets) = do
    transMap <- gets transition2id
    lv <- gets abstractionLv
    let id = findVariable name (findVariable lv transMap)
    let pcnt = map (\l -> (head l, length l)) (group (sort params))
    let pmap = Map.fromList pcnt
    let rmap = foldl' (\acc t -> Map.insertWith (+) t (-1) acc) pmap rets
    let rcnt = Map.toList rmap

    -- add hoparam places, ignore their initial state but set final state to zero
    mapM_ (addPlaceVar . funName) hoParams
    mapM_ (emptyPlace . funName) hoParams 

    return (FoldedFunction id pcnt rcnt)
  where
    emptyPlace p = do
        l <- loc <$> get
        placeMap <- place2variable <$> get
        tVar <- mkZ3IntVar $ findVariable (p, l) placeMap
        mkIntNum 0 >>= mkEq tVar >>= assert
