{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PetriNet.PNEncoder where

import Data.Maybe
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Z3.Monad hiding(Z3Env, newEnv)
import qualified Z3.Base as Z3
import Control.Monad.State

import PetriNet.PNBuilder
import Synquid.Util

data VarType = VarPlace | VarTransition | VarFlow

data Variable = Variable {
  varId :: Int,
  varName :: String,
  varTimestamp :: Int,
  varValue :: Int,
  varType :: VarType
}

data Z3Env = Z3Env {
  envSolver  :: Z3.Solver,
  envContext :: Z3.Context,
  envOptimize :: Z3.Optimize
}

newEnv :: Maybe Logic -> Opts -> IO Z3Env
newEnv mbLogic opts =
  Z3.withConfig $ \cfg -> do
    setOpts cfg opts
    ctx <- Z3.mkContext cfg
    opt <- Z3.mkOptimize ctx
    solver <- maybe (Z3.mkSolver ctx) (Z3.mkSolverForLogic ctx) mbLogic
    return $ Z3Env solver ctx opt

initialZ3Env = newEnv Nothing stdOpts

data EncodeState = EncodeState {
  z3env :: Z3Env,
  petriNet :: PetriNet,
  loc :: Int,
  nbVariable :: Int,
  place2variable :: HashMap (Place, Int, Int) Variable,
  transition2variable :: HashMap (Transition, Int) Variable,
  id2variable :: HashMap Int Variable,
  mustFirers :: [Id]
}

type Encoder = StateT EncodeState IO

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . z3env)
    getContext = gets (envContext . z3env)
    getOptimize = gets (envOptimize . z3env)

-- | create a new encoder in z3
createEncoder :: Encoder ()
createEncoder = do
    -- create all the type variables for encoding
    createVariables
    -- add all the constraints for the solver
    -- createConstraints

solveAndGetModel :: Encoder [(Transition, Int)]
solveAndGetModel = do
    -- res <- optimizeCheck
    let res = Sat
    case res of
        Sat -> do
            boolS <- mkBoolSort
            sym <- mkStringSymbol "x"
            var <- mkConst sym boolS
            assert var
            solverCheck
            model <- solverGetModel
            {- transMap <- transition2variable <$> get
            selected <- filterM (checkTransition model) $ HashMap.toList transMap
            return $ fst $ unzip selected -}
            bMay <- eval model var
            case bMay of
                Just b -> do
                    bStr <- astToString b
                    error $ bStr
                Nothing -> error "model check fails" 
        _ -> do
            liftIO $ print "unsat"
            return []
  where
    checkTransition model ((tr, t), v) = do
        trVar <- mkZ3Var v
        varStr <- astToString trVar
        bMay <- eval model trVar
        case bMay of
            Just b -> do
                bStr <- astToString b
                error $ bStr
            Nothing -> error $ "cannot eval the variable" ++ show (tr,t)

encoderSolve :: PetriNet -> Int -> [Id] -> IO ()
encoderSolve net loc hoArgs = do
    z3Env <- initialZ3Env
    let initialState = EncodeState z3Env net loc 1 HashMap.empty HashMap.empty HashMap.empty hoArgs
    evalStateT (createEncoder >> solveAndGetModel) initialState >>= print

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


-- | at each timestamp, only one transition can be fired
sequentialTransitions ::  Encoder ()
sequentialTransitions = do
    transitions <- HashMap.elems . pnTransitions . petriNet <$> get
    l <- loc <$> get
    mapM_ (fireAt transitions) [0..(l-1)]
  where
    fireAt transitions t = do
        transMap <- transition2variable <$> get
        let transVar = map (\tr -> HashMap.lookup (tr,t) transMap) transitions
        transZ3Var <- mapM (mkZ3Var . fromJust) transVar
        mapM_ (fireIth transZ3Var) [0..(length transitions - 1)]

    fireIth transitions i = do
        let otherTrans = deleteAt i transitions
        allOtherTrans <- mapM mkNot otherTrans >>= mkAnd
        mkImplies (transitions !! i) allOtherTrans >>= optimizeAssert

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
        let placeVars = map (\v -> HashMap.lookup (p, t, v) placeMap) [0..mt]
        placeZ3Vars <- mapM (mkZ3Var . fromJust) placeVars
        mapM_ (placeHasT placeZ3Vars) [0..mt]

    placeHasT vars v = do
        let otherTokens = deleteAt v vars
        allOtherTokens <- mapM mkNot otherTokens >>= mkAnd
        mkImplies (vars !! v) allOtherTokens >>= optimizeAssert

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
        let transitions = map (\i -> fromJust $ HashMap.lookup i trans) 
                        $ Set.toList $ placePreset p `Set.union` placePostset p
        transMap <- transition2variable <$> get
        let trVars = map (\tr -> HashMap.lookup (tr, t) transMap) transitions
        trZ3Vars <- mapM (mkZ3Var . fromJust) trVars
        mapM_ (noFireToken t p trZ3Vars) [0..(placeMaxToken p)]
        
    noFireToken t p trVars v = do
        placeMap <- place2variable <$> get
        curr <- mkZ3Var $ fromJust $ HashMap.lookup (p, t, v) placeMap
        next <- mkZ3Var $ fromJust $ HashMap.lookup (p, t + 1, v) placeMap
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
        let p = fromJust $ HashMap.lookup (flowPlace f) places
        let w = flowWeight f
        placeMap <- place2variable <$> get
        let satPlaces = map (\v -> fromJust $ HashMap.lookup (p,t,v) placeMap) 
                            [w..(placeMaxToken p)]
        mapM mkZ3Var satPlaces >>= mkOr >>= mkImplies trVar >>= optimizeAssert

    -- whether the src and dst places in the current tr is the same or "void"
    hasComplementary places p postFlow preFlow =   
        placeId p == "void" || 
       (fromJust (HashMap.lookup (flowPlace preFlow) places) == p && 
        flowWeight preFlow == flowWeight postFlow)

    -- if the dest place has reached its maximum token, we cannot fire the tr
    hasReachedMax t tr trVar preFlows f = do
        places <- pnPlaces . petriNet <$> get
        let p = fromJust $ HashMap.lookup (flowPlace f) places
        let w1 = flowWeight f
        placeMap <- place2variable <$> get
        maxTokenVar <- mkZ3Var $ fromJust 
                               $ HashMap.lookup (p,t,placeMaxToken p) placeMap
        if foldr ((||) . (hasComplementary places p f)) False preFlows
            then return ()
            else mkNot maxTokenVar >>= mkImplies trVar >>= optimizeAssert

    fireFor t tr = do
        flows <- pnFlows . petriNet <$> get
        transMap <- transition2variable <$> get
        trVar <- mkZ3Var $ fromJust $ HashMap.lookup (tr, t) transMap
        let preFlows = map (\f -> fromJust $ HashMap.lookup f flows)
                           (Set.toList $ transitionPreset tr)
        mapM_ (getSatisfiedPlaces t trVar) preFlows
        let postFlows = map (\f -> fromJust $ HashMap.lookup f flows)
                            (Set.toList $ transitionPostset tr)
        mapM_ (hasReachedMax t tr trVar preFlows) postFlows

postconditionsTransitions ::  Encoder ()
postconditionsTransitions = do
    l <- loc <$> get
    trans <- HashMap.elems . pnTransitions . petriNet <$> get
    mapM_ (uncurry placesToChange) $ zip [0..(l-1)] trans
  where
    addChangedPlace places pre f changeList = 
        let pid = flowPlace f
            p = fromJust $ HashMap.lookup pid places
            w = if pre then flowWeight f else - (flowWeight f)
        in HashMap.insert p (w + HashMap.lookupDefault 0 p changeList) changeList

    connectBefAft t trVar p diff x = do
        placeMap <- place2variable <$> get
        before <- mkZ3Var $ fromJust $ HashMap.lookup (p,t,x) placeMap
        after <- mkZ3Var $ fromJust $ HashMap.lookup (p,t+1,x+diff) placeMap
        mkAnd [before, trVar] >>= flip mkImplies after >>= optimizeAssert

    mkChange t trVar p diff = do
        placeMap <- place2variable <$> get
        let validTokens = [ x | x <- [0..(placeMaxToken p)]
                              , (x+diff > 0 && x+diff <= placeMaxToken p) 
                              || placeId p == "void" ]
        mapM_ (connectBefAft t trVar p diff) validTokens

    placesToChange t tr = do
        flows <- pnFlows . petriNet <$> get
        places <- pnPlaces . petriNet <$> get
        let preFlows = map (\f -> fromJust $ HashMap.lookup f flows)
                           (Set.toList $ transitionPreset tr)
        let preChange = foldr (addChangedPlace places True) HashMap.empty preFlows
        let postFlows = map (\f -> fromJust $ HashMap.lookup f flows)
                            (Set.toList $ transitionPostset tr)
        let toChange = foldr (addChangedPlace places False) preChange postFlows
        transMap <- transition2variable <$> get
        trVar <- mkZ3Var $ fromJust $ HashMap.lookup (tr, t) transMap
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
        trVars <- mapM (\t -> mkZ3Var . fromJust $ HashMap.lookup (tr, t) transMap)
                     $ [0..(l-1)]
        mkOr trVars >>= optimizeAssert
        
