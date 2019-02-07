{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PetriNet.PNEncoder(
      EncodeState(..)
    , SplitInfo(..)
    , encoderInit
    , encoderSolve
    , encoderRefine
    ) where

import Data.Maybe
import Data.List
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Z3.Monad hiding(Z3Env, newEnv)
import qualified Z3.Base as Z3
import Control.Monad.State
import System.CPUTime
import Text.Printf

import PetriNet.PNBuilder
import PetriNet.Encoder
import PetriNet.AbstractType
import Synquid.Util

type Encoder = StateT EncodeState IO

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . z3env)
    getContext = gets (envContext . z3env)
    getOptimize = gets (envOptimize . z3env)

-- | create a new encoder in z3
createEncoder :: [Id] -> Id -> Encoder ()
createEncoder inputs ret = do
    -- create all the type variables for encoding
    createVariables
    -- add all the constraints for the solver
    createConstraints
    -- set initial and final state for solver
    places <- pnPlaces . petriNet <$> get
    setInitialState inputs places
    setFinalState ret places

-- | set the initial state for the solver, where we have tokens only in void or inputs
-- the tokens in the other places should be zero
setInitialState :: [Id] -> HashMap Id Place -> Encoder ()
setInitialState inputs places = do
    let nonInputs = HashMap.filterWithKey (\k _ -> notElem k inputs) places
    mapM_ emptyOther $ HashMap.elems nonInputs
    -- assign tokens to each input types
    mapM_ (uncurry assignToken) inputCounts
  where
    inputCounts = map (\t -> (head t, length t)) $ group $ sort inputs
    assignToken t v = do
        places <- pnPlaces . petriNet <$> get
        placeMap <- place2variable <$> get
        let p = findVariable t places
        tVar <- mkZ3IntVar $ findVariable (placeId p, 0) placeMap
        mkIntNum v >>= mkEq tVar >>= assert
    emptyOther p = do 
        placeMap <- place2variable <$> get
        let v = if placeId p == "void" then 1 else 0
        tVar <- mkZ3IntVar $ findVariable (placeId p, 0) placeMap
        mkIntNum v >>= mkEq tVar >>= assert

-- | set the final solver state, we allow only one token in the return type
-- and maybe several tokens in the "void" place
setFinalState :: Id -> HashMap Id Place -> Encoder ()
setFinalState ret places = do
    -- the return value should have only one token
    includeRet
    -- other places excluding void and ret should have nothing
    let nonOutputs = HashMap.filterWithKey (\k _ -> k /= ret) places
    mapM_ excludeOther $ HashMap.elems nonOutputs
  where
    includeRet = do
        placeMap <- place2variable <$> get
        places <- pnPlaces . petriNet <$> get
        let retPlace = findVariable ret places
        l <- loc <$> get
        retVar <- mkZ3IntVar $ findVariable (placeId retPlace, l) placeMap
        mkIntNum 1 >>= mkEq retVar >>= assert
    excludeOther p = do
        l <- loc <$> get
        placeMap <- place2variable <$> get
        when (placeId p /= "void") $ do
            tVar <- mkZ3IntVar $ findVariable (placeId p, l) placeMap
            mkIntNum 0 >>= mkEq tVar >>= assert

solveAndGetModel :: Encoder [(Id, Int)]
solveAndGetModel = do
    res <- check
    l <- loc <$> get
        {- when (l==2) (do
        str <- solverToString
        liftIO (putStrLn str)
        transMap <- transition2variable <$> get
        liftIO (print transMap)
        placeMap <- place2variable <$> get
        liftIO (print placeMap)
                )
       -}
    case res of
        Sat -> do
            model <- solverGetModel
            transMap <- transition2variable <$> get
            selected <- filterM (checkLit model) $ HashMap.toList transMap
            let selectedTr = fst $ unzip selected
            blockTr <- mapM (\t -> mkZ3BoolVar $ findVariable t transMap) selectedTr
            -- placeMap <- place2variable <$> get
            -- blockP <- mapM (checkIntLit model) $ HashMap.toList placeMap
            mkAnd blockTr >>= mkNot >>= assert
            return selectedTr
        Unsat -> do
            liftIO $ print "unsat"
            return []
        Undef -> do
            liftIO $ print "undef"
            return []
  where
    checkLit model (k, v) = do
        trVar <- mkZ3BoolVar v
        bMay <- evalBool model trVar
        case bMay of
            Just b -> return b
            Nothing -> error $ "cannot eval the variable" ++ show k

    checkIntLit model (k, v) = do
        pVar <- mkZ3IntVar v
        iMay <- eval model pVar
        case iMay of
            Just i -> mkEq pVar i
            Nothing -> error $ "cannot eval the variable" ++ show k

encoderInit :: PetriNet -> Int -> [Id] -> [Id] -> Id -> IO EncodeState
encoderInit net loc hoArgs inputs ret = do
    z3Env <- initialZ3Env
    let initialState = EncodeState z3Env net loc 1 HashMap.empty HashMap.empty HashMap.empty hoArgs HashMap.empty HashMap.empty
    execStateT (createEncoder inputs ret) initialState

encoderSolve :: EncodeState -> IO ([(Id, Int)], EncodeState)
encoderSolve st = runStateT solveAndGetModel st

data SplitInfo = SplitInfo {
    oldPlace :: AbstractSkeleton,
    newPlace :: [AbstractSkeleton],
    splitedGroup :: [(Id, [Id])]
} deriving (Eq, Ord, Show)

updateParallelInfo :: SplitInfo -> Encoder ()
updateParallelInfo info = do
    trChd <- transitionChildren <$> get
    trPar <- transitionParents <$> get
    let splitTrans = splitedGroup info
    let trChd' = foldr (uncurry updateChildren) trChd splitTrans
    let trPar' = foldr (uncurry updateParents) trPar splitTrans
    st <- get
    put $ st { transitionParents = trPar'
             , transitionChildren = trChd' }
  where
    updateChildren splitedTrans splitToTrans transMap = 
        let includeSplited = HashMap.filter (elem splitedTrans) transMap
            fold_fun k _ = HashMap.insertWith (++) k splitToTrans
            transMap' = HashMap.foldrWithKey fold_fun transMap includeSplited
        in HashMap.insertWith (++) splitedTrans splitToTrans transMap'

    updateParents splitedTrans splitToTrans transMap =
        let parents = HashMap.lookupDefault [] splitedTrans transMap
            fold_fun tid m = HashMap.insertWith (++) tid (splitedTrans:parents) m
        in foldr fold_fun transMap splitToTrans

-- | wrap some action with time measuring and print out the execution time
withTime :: String -> Encoder a -> Encoder a
withTime desc f = do
    start <- liftIO getCPUTime
    res <- f
    end <- liftIO getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    -- let time = printf "%s time: %0.3f sec\n" desc (diff :: Double)
    -- liftIO $ putStrLn time
    return res

encoderRefine :: PetriNet -> SplitInfo -> [Id] -> Id -> Encoder ()
encoderRefine net info inputs ret = do
    -- update the transition hierarchy information
    updateParallelInfo info
    -- put the new petri net into the state
    -- oldNet <- petriNet <$> get
    st <- get
    put $ st { petriNet = net }
    let splitedTyp = show (oldPlace info)
    let typIds = map show (newPlace info)
    let splitedPlace = fromJust (HashMap.lookup splitedTyp (pnPlaces net))
    let newPlaces = map (\nt -> fromJust (HashMap.lookup nt (pnPlaces net))) typIds
    let inputClones = map (\n -> n ++ "|clone") inputs
    let transIds = concat (snd (unzip (splitedGroup info)))
    let newTrans = map (\tr -> fromJust (HashMap.lookup tr (pnTransitions net))) (inputClones ++ transIds)
    let lookupTrans tr = fromJust (HashMap.lookup tr (pnTransitions net))
    let splitTrans = map (\(tr, trs) -> (lookupTrans tr, map lookupTrans trs)) (splitedGroup info)

    l <- loc <$> get
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- newTrans ]

    -- add new place and transition variables
    mapM_ addPlaceVar newPlaces
    mapM_ addTransitionVar newTrans

    -- refine the sequential constraints
    -- but enable transitions between abstraction levels fired simultaneously
    -- let transitions = HashMap.elems (pnTransitions net)
    -- mapM_ (fireAt transitions) [0..(l-1)]
    withTime "sequentialTransitions" sequentialTransitions


    -- refine the precondition constraints
    withTime "preconditions" $ mapM_ (uncurry preconditionsTransitions) allTrans

    -- refine the postcondition constraints
    withTime "postconditions" $ mapM_ (uncurry postconditionsTransitions) allTrans

    -- refine the no transition constraints
    withTime "placeMerge" $ mapM_ (placeMerge splitedPlace newPlaces) [0..l]
    withTime "transMerge" $ mapM_ (uncurry transMerge) [(tr, t) | tr <- splitTrans, t <- [0..(l-1)]]
    withTime "noTransitionTokens" $ mapM_ (uncurry noTransitionTokens) [(t, p) | p <- newPlaces, t <- [0..(l-1)]]

    -- refine the must firers
    withTime "mustFireTransitions" mustFireTransitions

    -- set new initial and final state
    let newPmap = HashMap.filterWithKey (\k _ -> k `elem` typIds) (pnPlaces net)
    setInitialState inputs newPmap
    setFinalState ret newPmap
  where
    
    placeMerge p nps t = do
        placeMap <- place2variable <$> get
        let npVars = map (\np -> findVariable (placeId np, t) placeMap) nps
        npZ3Vars <- mapM mkZ3IntVar npVars
        let pVar = findVariable (placeId p, t) placeMap
        pZ3Var <- mkZ3IntVar pVar
        mkAdd npZ3Vars >>= mkEq pZ3Var >>= assert

    transMerge (tr, ntrs) t = do
        transMap <- transition2variable <$> get
        let ntrVars = map (\ntr -> findVariable (transitionId ntr, t) transMap) ntrs
        ntrZ3Vars <- mapM mkZ3BoolVar ntrVars
        let trVar = findVariable (transitionId tr, t) transMap
        trZ3Var <- mkZ3BoolVar trVar
        mkOr ntrZ3Vars >>= mkEq trZ3Var >>= assert

addPlaceVar ::  Place -> Encoder ()
addPlaceVar p = do
    l <- loc <$> get
    mapM_ addPlaceVarAt [0..l]
  where
    addPlaceVarAt t = do
        st <- get
        let placeVar = Variable (nbVariable st) (placeId p) t 0 VarPlace
        put $ st { place2variable = HashMap.insert (placeId p, t) 
                                                    placeVar 
                                                   (place2variable st)
                 , nbVariable = 1 + (nbVariable st)
                 }

addTransitionVar ::  Transition -> Encoder ()
addTransitionVar tr = do
    l <- loc <$> get
    mapM_ addTransitionVarAt [0..(l-1)]
  where
    addTransitionVarAt t = do
        st <- get
        let transitionVar = Variable (nbVariable st) (transitionId tr) t 0 VarTransition
        put $ st { transition2variable = HashMap.insert (transitionId tr, t)
                                                         transitionVar
                                                        (transition2variable st)
                 , id2variable = HashMap.insert (nbVariable st) 
                                                 transitionVar 
                                                (id2variable st)
                 , nbVariable = 1 + (nbVariable st)
                 }

-- | map each place and transition to a variable in z3
createVariables ::  Encoder ()
createVariables = do
    pn <- petriNet <$> get
    -- add place variables
    mapM_ addPlaceVar $ pnPlaces pn
    -- add transition variables
    mapM_ addTransitionVar $ pnTransitions pn

createConstraints ::  Encoder ()
createConstraints = do
    -- prepare constraint parameters
    l <- loc <$> get
    trans <- HashMap.elems . pnTransitions . petriNet <$> get
    places <- HashMap.elems . pnPlaces . petriNet <$> get
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- trans]
    let allPlaces = [(t, p) | t <- [0..(l-1)], p <- places]

    sequentialTransitions

    mapM_ (uncurry preconditionsTransitions) allTrans

    mapM_ (uncurry postconditionsTransitions) allTrans

    mapM_ (uncurry noTransitionTokens) allPlaces

    mustFireTransitions


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

-- | at each timestamp, only one transition can be fired
sequentialTransitions :: Encoder ()
sequentialTransitions = do
    transitions <- HashMap.elems . pnTransitions . petriNet <$> get
    l <- loc <$> get
    mapM_ (fireAt transitions) [0..(l-1)]
  where
    fireAt transitions t = do
        transMap <- transition2variable <$> get
        let transVar = map (\tr -> findVariable (transitionId tr, t) transMap) transitions
        allPossible <- mapM (fireIth transVar) [0..(length transitions - 1)]
        mkOr allPossible >>= assert

    fireIth transitions i = do
        trPar <- transitionParents <$> get
        trChd <- transitionChildren <$> get
        let tid = varName (transitions !! i)
        let excludeList = HashMap.lookupDefault [] tid trPar
                       ++ HashMap.lookupDefault [] tid trChd
                       ++ if "|clone" `isSuffixOf` tid 
                             then map varName $ filter (isSuffixOf "|clone" . varName) transitions 
                             else []
        let otherTrans = deleteAt i transitions
        let excludedTrans = filter (\tr -> not (elem (varName tr) excludeList)) otherTrans
        allOtherTrans <- mapM (mkZ3BoolVar >=> mkNot) excludedTrans
        firedTr <- mkZ3BoolVar (transitions !! i)
        mkAnd (firedTr : allOtherTrans)

-- | if this place has no connected transition fired, 
-- it has the same # of tokens
noTransitionTokens :: Int -> Place -> Encoder ()
noTransitionTokens t p = noFirePlace t p
  where
    noFirePlace t p = do
        trans <- pnTransitions . petriNet <$> get
        let transitions = map (\i -> findVariable i trans) 
                        $ Set.toList $ placePreset p `Set.union` placePostset p
        transMap <- transition2variable <$> get
        let trVars = map (\tr -> findVariable (transitionId tr, t) transMap) transitions
        trZ3Vars <- mapM mkZ3BoolVar trVars
        placeMap <- place2variable <$> get
        curr <- mkZ3IntVar $ findVariable (placeId p, t) placeMap
        next <- mkZ3IntVar $ findVariable (placeId p, t + 1) placeMap
        unfiredTrans <- mapM mkNot trZ3Vars >>= mkAnd
        mkEq curr next >>= mkImplies unfiredTrans >>= assert

-- | calculate the preconditions for each transition to be fired
preconditionsTransitions :: Int -> Transition -> Encoder ()
preconditionsTransitions t tr = fireFor t tr
  where
    -- get enough resources for current places to fire the tr
    getSatisfiedPlaces t trVar f = do
        places <- pnPlaces . petriNet <$> get
        let p = case HashMap.lookup (flowPlace f) places of
                    Just pp -> pp
                    Nothing -> error $ "cannot find place " ++ (flowPlace f)
        w <- mkIntNum $ flowWeight f
        placeMap <- place2variable <$> get
        pVar <- mkZ3IntVar $ findVariable (placeId p,t) placeMap
        mkGe pVar w >>= mkImplies trVar >>= assert

    -- whether the src and dst places in the current tr is the same or "void"
    hasComplementary places p postFlow preFlow =   
        placeId p == "void" || 
       ((findVariable (flowPlace preFlow) places) == p && 
        flowWeight preFlow >= flowWeight postFlow)

    -- if the dest place has reached its maximum token, we cannot fire the tr
    hasReachedMax t tr trVar preFlows f = do
        places <- pnPlaces . petriNet <$> get
        let p = findVariable (flowPlace f) places
        w1 <- mkIntNum $ placeMaxToken p
        placeMap <- place2variable <$> get
        maxTokenVar <- mkZ3IntVar $ findVariable (placeId p, t) placeMap
        if foldr ((||) . (hasComplementary places p f)) False preFlows
            then return ()
            else mkEq maxTokenVar w1 >>= mkNot >>= mkImplies trVar >>= assert

    fireFor t tr = do
        flows <- pnFlows . petriNet <$> get
        transMap <- transition2variable <$> get
        trVar <- mkZ3BoolVar $ findVariable (transitionId tr, t) transMap
        let preFlows = map (\f -> findVariable f flows)
                           (Set.toList $ transitionPreset tr)
        mapM_ (getSatisfiedPlaces t trVar) preFlows
        -- let postFlows = map (\f -> findVariable f flows)
        --                    (Set.toList $ transitionPostset tr)
        -- mapM_ (hasReachedMax t tr trVar preFlows) postFlows

postconditionsTransitions :: Int -> Transition -> Encoder ()
postconditionsTransitions t tr = placesToChange t tr
  where
    addChangedPlace places pre f changeList = 
        let pid = flowPlace f
            p = findVariable pid places 
            w = if pre then -(flowWeight f) else flowWeight f
        in HashMap.insertWith (+) p w changeList

    mkChange t trVar p diff = do
        let d = if placeId p == "void" then 0 else diff
        placeMap <- place2variable <$> get
        before <- mkZ3IntVar $ findVariable (placeId p, t) placeMap
        after <- mkZ3IntVar $ findVariable (placeId p, t + 1) placeMap
        diffw <- mkIntNum d
        mkAdd [before, diffw] >>= mkEq after >>= mkImplies trVar >>= assert

    placesToChange t tr = do
        flows <- pnFlows . petriNet <$> get
        places <- pnPlaces . petriNet <$> get
        let preFlows = map (\f -> findVariable f flows)
                           (Set.toList $ transitionPreset tr)
        let preChange = foldr (addChangedPlace places True) HashMap.empty preFlows
        let postFlows = map (\f -> findVariable f flows)
                            (Set.toList $ transitionPostset tr)
        let toChange = foldr (addChangedPlace places False) preChange postFlows
        transMap <- transition2variable <$> get
        trVar <- mkZ3BoolVar $ findVariable (transitionId tr, t) transMap
        -- liftIO $ print $ "prepare to fire " ++ show (tr, t)
        mapM_ (uncurry (mkChange t trVar)) (HashMap.toList toChange)

mustFireTransitions ::  Encoder ()
mustFireTransitions = do
    must <- mustFirers <$> get
    transitions <- HashMap.elems . pnTransitions . petriNet <$> get
    let mustTrans = filter (nameInMust must . transitionId) transitions
    mapM_ fireTransition mustTrans
  where
    nameInMust must name = foldr ((||) . flip isInfixOf name) False must
    fireTransition tr = do
        transMap <- transition2variable <$> get
        l <- loc <$> get
        trVars <- mapM (\t -> mkZ3BoolVar $ findVariable (transitionId tr, t) transMap) [0..(l-1)]
        mkOr trVars >>= assert
   
