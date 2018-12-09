{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PetriNet.PNEncoderSMT(
      EncodeSMTState(..)
    , encoderInitSMT
    , encoderSolveSMT
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

import PetriNet.PNBuilder
import PetriNet.Encoder
import Synquid.Util

type EncoderSMT = StateT EncodeSMTState IO

instance MonadZ3 EncoderSMT where
    getSolver = gets (envSolver . z3envSMT)
    getContext = gets (envContext . z3envSMT)
    getOptimize = gets (envOptimize . z3envSMT)

-- | create a new encoder in z3
createEncoderSMT :: [Id] -> Id -> EncoderSMT ()
createEncoderSMT inputs ret = do
    -- create all the type variables for encoding
    createVariables
    -- add all the constraints for the solver
    createConstraints
    -- set initial and final state for solver
    setInitialState inputs
    setFinalState ret

-- | set the initial state for the solver, where we have tokens only in void or inputs
-- the tokens in the other places should be zero
setInitialState :: [Id] -> EncoderSMT ()
setInitialState inputs = do
    places <- pnPlaces . petriNetSMT <$> get
    let nonInputs = HashMap.filterWithKey (\k _ -> notElem k inputs) places
    mapM_ emptyOther $ HashMap.elems nonInputs
    -- assign tokens to each input types
    mapM_ (uncurry assignToken) inputCounts
  where
    inputCounts = map (\t -> (head t, length t)) $ group $ sort inputs
    assignToken t v = do
        places <- pnPlaces . petriNetSMT <$> get
        placeMap <- place2variableSMT <$> get
        let p = case HashMap.lookup t places of
                    Just p -> p
                    Nothing -> error $ "cannot find place " ++ t
        tVar <- mkZ3IntVar $ case HashMap.lookup (p,0) placeMap of
                                  Just v -> v
                                  Nothing -> error $ "cannot find variable " ++ show (p,0)
        mkIntNum v >>= mkEq tVar >>= optimizeAssert
    emptyOther p = do 
        placeMap <- place2variableSMT <$> get
        let v = if placeId p == "void" then 1 else 0
        tVar <- mkZ3IntVar $ case HashMap.lookup (p,0) placeMap of
                                  Just v -> v
                                  Nothing -> error $ "cannot find variable" ++ show (p,v)
        mkIntNum v >>= mkEq tVar >>= optimizeAssert

-- | set the final solver state, we allow only one token in the return type
-- and maybe several tokens in the "void" place
setFinalState :: Id -> EncoderSMT ()
setFinalState ret = do
    -- the return value should have only one token
    includeRet
    -- other places excluding void and ret should have nothing
    places <- pnPlaces . petriNetSMT <$> get
    let nonOutputs = HashMap.filterWithKey (\k _ -> k /= ret) places
    mapM_ excludeOther $ HashMap.elems nonOutputs
  where
    includeRet = do
        places <- pnPlaces . petriNetSMT <$> get
        placeMap <- place2variableSMT <$> get
        let retPlace = findVariable ret places
        l <- locSMT <$> get
        retVar <- mkZ3IntVar $ findVariable (retPlace,l) placeMap
        mkIntNum 1 >>= mkEq retVar >>= optimizeAssert
    excludeOther p = do
        l <- locSMT <$> get
        placeMap <- place2variableSMT <$> get
        when (placeId p /= "void") $ do
            tVar <- mkZ3IntVar $ findVariable (p,l) placeMap
            mkIntNum 0 >>= mkEq tVar >>= optimizeAssert

solveAndGetModel :: EncoderSMT [(Transition, Int)]
solveAndGetModel = do
    res <- optimizeCheck
    l <- locSMT <$> get

    case res of
        Sat -> do
            model <- optimizeGetModel
            transMap <- transition2variableSMT <$> get
            selected <- filterM (checkLit model) $ HashMap.toList transMap
            let selectedTr = fst $ unzip selected
            blockTr <- mapM (\t -> mkZ3BoolVar $ findVariable t transMap) selectedTr
            placeMap <- place2variableSMT <$> get
            blockP <- mapM (checkIntLit model) $ HashMap.toList placeMap
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

encoderInitSMT :: PetriNet -> Int -> [Id] -> [Id] -> Id -> IO EncoderType
encoderInitSMT net locSMT hoArgs inputs ret = do
    z3Env <- initialZ3Env
    let initialState = EncodeSMTState z3Env net locSMT 1 HashMap.empty HashMap.empty HashMap.empty hoArgs
    Right <$> execStateT (createEncoderSMT inputs ret) initialState

encoderSolveSMT :: EncodeSMTState -> IO ([(Transition, Int)], EncoderType)
encoderSolveSMT st = do
    (a, s) <- runStateT solveAndGetModel st
    return (a, Right s)

addPlaceVar ::  Place -> EncoderSMT ()
addPlaceVar p = do
    l <- locSMT <$> get
    mapM_ addPlaceVarAt [0..l]
  where
    addPlaceVarAt t = do
        st <- get
        let placeVar = Variable (nbVariableSMT st) (placeId p) t 0 VarPlace
        put $ st { place2variableSMT = HashMap.insert (p, t) 
                                                    placeVar 
                                                   (place2variableSMT st)
                 , nbVariableSMT = 1 + (nbVariableSMT st)
                 }

addTransitionVar ::  Transition -> EncoderSMT ()
addTransitionVar tr = do
    l <- locSMT <$> get
    mapM_ addTransitionVarAt [0..(l-1)]
  where
    addTransitionVarAt t = do
        st <- get
        let transitionVar = Variable (nbVariableSMT st) (transitionId tr) t 0 VarTransition
        put $ st { transition2variableSMT = HashMap.insert (tr, t)
                                                         transitionVar
                                                        (transition2variableSMT st)
                 , id2variableSMT = HashMap.insert (nbVariableSMT st) 
                                                 transitionVar 
                                                (id2variableSMT st)
                 , nbVariableSMT = 1 + (nbVariableSMT st)
                 }

-- | map each place and transition to a variable in z3
createVariables ::  EncoderSMT ()
createVariables = do
    pn <- petriNetSMT <$> get
    -- add place variables
    mapM_ addPlaceVar $ pnPlaces pn
    -- add transition variables
    mapM_ addTransitionVar $ pnTransitions pn

createConstraints ::  EncoderSMT ()
createConstraints = do

    sequentialTransitions

    -- tokenRestrictions

    preconditionsTransitions

    postconditionsTransitions

    noTransitionTokens

    mustFireTransitions


mkZ3BoolVar ::  Variable -> EncoderSMT AST
mkZ3BoolVar var = do
    varSymbol <- mkIntSymbol (varId var)
    boolS <- mkBoolSort
    const <- mkConst varSymbol boolS
    return const

mkZ3IntVar :: Variable -> EncoderSMT AST
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
sequentialTransitions ::  EncoderSMT ()
sequentialTransitions = do
    transitions <- HashMap.elems . pnTransitions . petriNetSMT <$> get
    l <- locSMT <$> get
    mapM_ (fireAt transitions) [0..(l-1)]
  where
    fireAt transitions t = do
        transMap <- transition2variableSMT <$> get
        let transVar = map (\tr -> findVariable (tr,t) transMap) transitions
        transZ3Var <- mapM mkZ3BoolVar transVar
        allPossible <- mapM (fireIth transZ3Var) [0..(length transitions - 1)]
        mkOr allPossible >>= optimizeAssert

    fireIth transitions i = do
        let otherTrans = deleteAt i transitions
        allOtherTrans <- mapM mkNot otherTrans
        mkAnd ((transitions !! i):allOtherTrans)
        -- mkImplies (transitions !! i) allOtherTrans >>= optimizeAssert

-- | if this place has no connected transition fired, 
-- it has the same # of tokens
noTransitionTokens ::  EncoderSMT ()
noTransitionTokens = do
    l <- locSMT <$> get
    mapM_ noFireAt [0..(l-1)]
  where
    noFireAt t = do
        places <- HashMap.elems . pnPlaces . petriNetSMT <$> get
        mapM_ (noFirePlace t) places

    noFirePlace t p = do
        trans <- pnTransitions . petriNetSMT <$> get
        let transitions = map (\i -> findVariable i trans) 
                        $ Set.toList $ placePreset p `Set.union` placePostset p
        transMap <- transition2variableSMT <$> get
        let trVars = map (\tr -> findVariable (tr, t) transMap) transitions
        trZ3Vars <- mapM mkZ3BoolVar trVars
        placeMap <- place2variableSMT <$> get
        curr <- mkZ3IntVar $ findVariable (p, t) placeMap
        next <- mkZ3IntVar $ findVariable (p, t + 1) placeMap
        unfiredTrans <- mapM mkNot trZ3Vars >>= mkAnd
        mkEq curr next >>= mkImplies unfiredTrans >>= optimizeAssert

-- | calculate the preconditions for each transition to be fired
preconditionsTransitions ::  EncoderSMT ()
preconditionsTransitions = do
    l <- locSMT <$> get
    mapM_ fireAt [0..(l-1)]
  where
    fireAt t = do
        transitions <- HashMap.elems . pnTransitions . petriNetSMT <$> get
        mapM_ (fireFor t) transitions

    -- get enough resources for current places to fire the tr
    getSatisfiedPlaces t trVar f = do
        places <- pnPlaces . petriNetSMT <$> get
        let p = case HashMap.lookup (flowPlace f) places of
                    Just pp -> pp
                    Nothing -> error $ "cannot find place " ++ (flowPlace f)
        w <- mkIntNum $ flowWeight f
        placeMap <- place2variableSMT <$> get
        pVar <- mkZ3IntVar $ findVariable (p,t) placeMap
        mkGe pVar w >>= mkImplies trVar >>= optimizeAssert

    -- whether the src and dst places in the current tr is the same or "void"
    hasComplementary places p postFlow preFlow =   
        placeId p == "void" || 
       ((findVariable (flowPlace preFlow) places) == p && 
        flowWeight preFlow == flowWeight postFlow)

    -- if the dest place has reached its maximum token, we cannot fire the tr
    hasReachedMax t tr trVar preFlows f = do
        places <- pnPlaces . petriNetSMT <$> get
        let p = findVariable (flowPlace f) places
        w1 <- mkIntNum $ flowWeight f
        placeMap <- place2variableSMT <$> get
        maxTokenVar <- mkZ3IntVar $ findVariable (p,t) placeMap
        if foldr ((||) . (hasComplementary places p f)) False preFlows
            then return ()
            else mkEq maxTokenVar w1 >>= mkNot >>= mkImplies trVar >>= optimizeAssert

    fireFor t tr = do
        flows <- pnFlows . petriNetSMT <$> get
        transMap <- transition2variableSMT <$> get
        trVar <- mkZ3BoolVar $ findVariable (tr, t) transMap
        let preFlows = map (\f -> findVariable f flows)
                           (Set.toList $ transitionPreset tr)
        mapM_ (getSatisfiedPlaces t trVar) preFlows
        let postFlows = map (\f -> findVariable f flows)
                            (Set.toList $ transitionPostset tr)
        mapM_ (hasReachedMax t tr trVar preFlows) postFlows

postconditionsTransitions ::  EncoderSMT ()
postconditionsTransitions = do
    l <- locSMT <$> get
    trans <- HashMap.elems . pnTransitions . petriNetSMT <$> get
    -- liftIO $ print $ "all transitions:" ++ show trans 
    mapM_ (uncurry placesToChange) $ [(t, tr) | t <- [0..(l-1)], tr <- trans]
  where
    addChangedPlace places pre f changeList = 
        let pid = flowPlace f
            p = findVariable pid places 
            w = if pre then -(flowWeight f) else flowWeight f
        in HashMap.insertWith (+) p w changeList

    mkChange t trVar p diff = do
        let d = if placeId p == "void" then 0 else diff
        placeMap <- place2variableSMT <$> get
        before <- mkZ3IntVar $ findVariable (p, t) placeMap
        after <- mkZ3IntVar $ findVariable (p, t + 1) placeMap
        diffw <- mkIntNum d
        mkAdd [before, diffw] >>= mkEq after >>= mkImplies trVar >>= optimizeAssert

    placesToChange t tr = do
        flows <- pnFlows . petriNetSMT <$> get
        places <- pnPlaces . petriNetSMT <$> get
        let preFlows = map (\f -> findVariable f flows)
                           (Set.toList $ transitionPreset tr)
        let preChange = foldr (addChangedPlace places True) HashMap.empty preFlows
        let postFlows = map (\f -> findVariable f flows)
                            (Set.toList $ transitionPostset tr)
        let toChange = foldr (addChangedPlace places False) preChange postFlows
        transMap <- transition2variableSMT <$> get
        trVar <- mkZ3BoolVar $ findVariable (tr, t) transMap
        -- liftIO $ print $ "prepare to fire " ++ show (tr, t)
        mapM_ (uncurry (mkChange t trVar)) (HashMap.toList toChange)

mustFireTransitions ::  EncoderSMT ()
mustFireTransitions = do
    must <- mustFirersSMT <$> get
    transitions <- HashMap.elems . pnTransitions . petriNetSMT <$> get
    let mustTrans = filter (nameInMust must . transitionId) transitions
    mapM_ fireTransition mustTrans
  where
    nameInMust must name = foldr ((||) . flip isInfixOf name) False must
    fireTransition tr = do
        transMap <- transition2variableSMT <$> get
        l <- locSMT <$> get
        trVars <- mapM (\t -> mkZ3BoolVar $ findVariable (tr, t) transMap) [0..(l-1)]
        mkOr trVars >>= optimizeAssert
   