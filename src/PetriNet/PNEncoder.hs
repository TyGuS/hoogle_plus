{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PetriNet.PNEncoder(
      EncodeState(..)
    , encoderInit
    , encoderSolve
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

import PetriNet.Encoder hiding(Encoder)
import PetriNet.PNBuilder
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
    setInitialState inputs
    setFinalState ret

-- | set the initial state for the solver, where we have tokens only in void or inputs
-- the tokens in the other places should be zero
setInitialState :: [Id] -> Encoder ()
setInitialState inputs = do
    places <- pnPlaces . petriNet <$> get
    let nonInputs = HashMap.filterWithKey (\k _ -> notElem k inputs) places
    mapM_ emptyOther $ HashMap.elems nonInputs
    -- assign tokens to each input types
    mapM_ (uncurry assignToken) inputCounts
  where
    inputCounts = map (\t -> (head t, length t)) $ group $ sort inputs
    assignToken t v = do
        places <- pnPlaces . petriNet <$> get
        placeMap <- place2variable <$> get
        let p = case HashMap.lookup t places of
                    Just p -> p
                    Nothing -> error $ "cannot find place " ++ t
        tVar <- mkZ3Var $ case HashMap.lookup (p,0,v) placeMap of
                              Just v -> v
                              Nothing -> error $ "cannot find variable " ++ show (p,0,v)
        optimizeAssert tVar
    emptyOther p = do
        placeMap <- place2variable <$> get
        let v = if placeId p == "void" then 1 else 0
        tVar <- mkZ3Var $ case HashMap.lookup (p,0,v) placeMap of
                              Just v -> v
                              Nothing -> error $ "cannot find variable" ++ show (p,0,v)
        optimizeAssert tVar

-- | set the final solver state, we allow only one token in the return type
-- and maybe several tokens in the "void" place
setFinalState :: Id -> Encoder ()
setFinalState ret = do
    -- the return value should have only one token
    includeRet
    -- other places excluding void and ret should have nothing
    places <- pnPlaces . petriNet <$> get
    let nonOutputs = HashMap.filterWithKey (\k _ -> k /= ret) places
    mapM_ excludeOther $ HashMap.elems nonOutputs
  where
    includeRet = do
        places <- pnPlaces . petriNet <$> get
        placeMap <- place2variable <$> get
        let retPlace = findVariable ret places
        l <- loc <$> get
        retVar <- mkZ3Var $ findVariable (retPlace,l,1) placeMap
        optimizeAssert retVar
    excludeOther p = do
        l <- loc <$> get
        placeMap <- place2variable <$> get
        when (placeId p /= "void") $ do
            tVar <- mkZ3Var $ findVariable (p,l,0) placeMap
            optimizeAssert tVar

solveAndGetModel :: Encoder [(Transition, Int)]
solveAndGetModel = do
    res <- optimizeCheck
    l <- loc <$> get

    case res of
        Sat -> do
            model <- optimizeGetModel
            transMap <- transition2variable <$> get
            selected <- filterM (checkLit model) $ HashMap.toList transMap
            let selectedTr = fst $ unzip selected
            blockTr <- mapM (\t -> mkZ3Var $ findVariable t transMap) selectedTr
            placeMap <- place2variable <$> get
            selectedPlaces <- filterM (checkLit model) $ HashMap.toList placeMap
            let selectedP = fst $ unzip selectedPlaces
            blockP <- mapM (\p -> mkZ3Var $ findVariable p placeMap) selectedP
            mkAnd (blockTr ++ blockP) >>= mkNot >>= optimizeAssert
            return selectedTr
        Unsat -> do
            liftIO $ print "unsat"
            return []
        Undef -> do
            liftIO $ print "undef"
            return []
  where
    checkLit model (k, v) = do
        trVar <- mkZ3Var v
        bMay <- evalBool model trVar
        case bMay of
            Just b -> return b
            Nothing -> error $ "cannot eval the variable" ++ show k

encoderInit :: PetriNet -> Int -> [Id] -> [Id] -> Id -> IO EncoderType
encoderInit net loc hoArgs inputs ret = do
    z3Env <- initialZ3Env
    let initialState = EncodeState z3Env net loc 1 HashMap.empty HashMap.empty HashMap.empty hoArgs
    Left <$> execStateT (createEncoder inputs ret) initialState

encoderSolve :: EncodeState -> IO ([(Transition, Int)], EncoderType)
encoderSolve st = do
    (a, s) <- runStateT solveAndGetModel st
    return (a, Left s)

addPlaceVar ::  Place -> Encoder ()
addPlaceVar p = do
    l <- loc <$> get
    mapM_ (uncurry addPlaceVarAt) $ [(t,v) | t <- [0..l]
                                           , v <- [0..(placeMaxToken p)] ]
  where
    addPlaceVarAt t v = do
        st <- get
        let placeVar = Variable (nbVariable st) (placeId p) t v VarPlace
        put $ st { place2variable = HashMap.insert (p, t, v) 
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
        put $ st { transition2variable = HashMap.insert (tr, t)
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

    sequentialTransitions

    tokenRestrictions

    preconditionsTransitions

    postconditionsTransitions

    noTransitionTokens

    mustFireTransitions

mkZ3Var ::  Variable -> Encoder AST
mkZ3Var var = do
    varSymbol <- mkIntSymbol (varId var)
    boolS <- mkBoolSort
    const <- mkConst varSymbol boolS
    return const

findVariable :: (Eq k, Hashable k, Show k) => k -> HashMap k v -> v
findVariable k m = case HashMap.lookup k m of
                        Just v -> v
                        Nothing -> error $ "cannot find variable for " ++ show k

-- | at each timestamp, only one transition can be fired
sequentialTransitions ::  Encoder ()
sequentialTransitions = do
    transitions <- HashMap.elems . pnTransitions . petriNet <$> get
    l <- loc <$> get
    mapM_ (fireAt transitions) [0..(l-1)]
  where
    fireAt transitions t = do
        transMap <- transition2variable <$> get
        let transVar = map (\tr -> findVariable (tr,t) transMap) transitions
        transZ3Var <- mapM mkZ3Var transVar
        allPossible <- mapM (fireIth transZ3Var) [0..(length transitions - 1)]
        mkOr allPossible >>= optimizeAssert

    fireIth transitions i = do
        let otherTrans = deleteAt i transitions
        allOtherTrans <- mapM mkNot otherTrans
        mkAnd ((transitions !! i):allOtherTrans)
        -- mkImplies (transitions !! i) allOtherTrans >>= optimizeAssert

-- | at each timestamp, each place can only have some specific number of tokens
tokenRestrictions ::  Encoder ()
tokenRestrictions = do
    places <- HashMap.elems . pnPlaces . petriNet <$> get
    l <- loc <$> get
    mapM_ (singleAt places) [0..l]
  where
    singleAt places t = mapM_ (onlyOneToken t) places
        
    onlyOneToken t p = do
        placeMap <- place2variable <$> get
        let mt = placeMaxToken p 
        let placeVars = map (\v -> findVariable (p, t, v) placeMap) [0..mt]
        placeZ3Vars <- mapM mkZ3Var placeVars
        allPossible <- mapM (placeHasT placeZ3Vars) [0..mt]
        mkOr allPossible >>= optimizeAssert

    placeHasT vars v = do
        let otherTokens = deleteAt v vars
        allOtherTokens <- mapM mkNot otherTokens --  >>= mkAnd
        mkAnd ((vars !! v) : allOtherTokens)
        -- mkImplies (vars !! v) allOtherTokens >>= optimizeAssert

-- | if this place has no connected transition fired, 
-- it has the same # of tokens
noTransitionTokens ::  Encoder ()
noTransitionTokens = do
    l <- loc <$> get
    mapM_ noFireAt [0..(l-1)]
  where
    noFireAt t = do
        places <- HashMap.elems . pnPlaces . petriNet <$> get
        mapM_ (noFirePlace t) places

    noFirePlace t p = do
        trans <- pnTransitions . petriNet <$> get
        let transitions = map (\i -> findVariable i trans) 
                        $ Set.toList $ placePreset p `Set.union` placePostset p
        transMap <- transition2variable <$> get
        let trVars = map (\tr -> findVariable (tr, t) transMap) transitions
        trZ3Vars <- mapM (mkZ3Var) trVars
        mapM_ (noFireToken t p trZ3Vars) [0..(placeMaxToken p)]
        
    noFireToken t p trVars v = do
        placeMap <- place2variable <$> get
        curr <- mkZ3Var $ findVariable (p, t, v) placeMap
        next <- mkZ3Var $ findVariable (p, t + 1, v) placeMap
        unfiredTrans <- mapM mkNot trVars
        mkAnd (curr:unfiredTrans) >>= flip mkImplies next >>= optimizeAssert

-- | calculate the preconditions for each transition to be fired
preconditionsTransitions ::  Encoder ()
preconditionsTransitions = do
    l <- loc <$> get
    mapM_ fireAt [0..(l-1)]
  where
    fireAt t = do
        transitions <- HashMap.elems . pnTransitions . petriNet <$> get
        mapM_ (fireFor t) transitions

    -- get enough resources for current places to fire the tr
    getSatisfiedPlaces t trVar f = do
        places <- pnPlaces . petriNet <$> get
        let p = case HashMap.lookup (flowPlace f) places of
                    Just pp -> pp
                    Nothing -> error $ "cannot find place " ++ (flowPlace f)
        let w = flowWeight f
        placeMap <- place2variable <$> get
        let satPlaces = map (\v -> findVariable (p,t,v) placeMap) 
                            [w..(placeMaxToken p)]
        mapM mkZ3Var satPlaces >>= mkOr >>= mkImplies trVar >>= optimizeAssert

    -- whether the src and dst places in the current tr is the same or "void"
    hasComplementary places p postFlow preFlow =   
        placeId p == "void" || 
       ((findVariable (flowPlace preFlow) places) == p && 
        flowWeight preFlow == flowWeight postFlow)

    -- if the dest place has reached its maximum token, we cannot fire the tr
    hasReachedMax t tr trVar preFlows f = do
        places <- pnPlaces . petriNet <$> get
        let p = findVariable (flowPlace f) places
        let w1 = flowWeight f
        placeMap <- place2variable <$> get
        maxTokenVar <- mkZ3Var $ findVariable (p,t,placeMaxToken p) placeMap
        if foldr ((||) . (hasComplementary places p f)) False preFlows
            then return ()
            else mkNot maxTokenVar >>= mkImplies trVar >>= optimizeAssert

    fireFor t tr = do
        flows <- pnFlows . petriNet <$> get
        transMap <- transition2variable <$> get
        trVar <- mkZ3Var $ findVariable (tr, t) transMap
        let preFlows = map (\f -> findVariable f flows)
                           (Set.toList $ transitionPreset tr)
        mapM_ (getSatisfiedPlaces t trVar) preFlows
        let postFlows = map (\f -> findVariable f flows)
                            (Set.toList $ transitionPostset tr)
        mapM_ (hasReachedMax t tr trVar preFlows) postFlows

postconditionsTransitions ::  Encoder ()
postconditionsTransitions = do
    l <- loc <$> get
    trans <- HashMap.elems . pnTransitions . petriNet <$> get
    -- liftIO $ print $ "all transitions:" ++ show trans 
    mapM_ (uncurry placesToChange) $ [(t, tr) | t <- [0..(l-1)], tr <- trans]
  where
    addChangedPlace places pre f changeList = 
        let pid = flowPlace f
            p = findVariable pid places 
            w = if pre then -(flowWeight f) else flowWeight f
        in HashMap.insertWith (+) p w changeList

    connectBefAft t trVar p diff x = do
        placeMap <- place2variable <$> get
        before <- mkZ3Var $ findVariable (p, t, x) placeMap
        after <- mkZ3Var $ findVariable (p, t + 1, x + diff) placeMap
        -- liftIO $ print $ "(" ++ show (placeId p,t,x) ++ ") => " ++ show (placeId p, t+1, x+diff) 
        mkAnd [before, trVar] >>= flip mkImplies after >>= optimizeAssert

    mkChange t trVar p diff = do
        let validTokens = [ x | x <- [0..(placeMaxToken p)]
                              , (x + diff >= 0 && x + diff <= placeMaxToken p) 
                              || placeId p == "void" ]
        let d = if placeId p == "void" then 0 else diff
        mapM_ (connectBefAft t trVar p d) validTokens

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
        trVar <- mkZ3Var $ findVariable (tr, t) transMap
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
        trVars <- mapM (\t -> mkZ3Var $ findVariable (tr, t) transMap) [0..(l-1)]
        mkOr trVars >>= optimizeAssert
        
