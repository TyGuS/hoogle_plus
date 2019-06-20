{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PetriNet.PNEncoder(
     encoderInit
    , encoderSolve
    , encoderRefine
    , encoderInc
    ) where

import Data.Maybe
import Data.List
import Data.List.Extra
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Z3.Monad hiding(Z3Env, newEnv)
import qualified Z3.Base as Z3
import Control.Monad.State
import System.CPUTime
import Text.Printf
import Data.Text (pack, unpack, replace)
import System.IO

import Types.Common
import Types.Encoder
import Types.Abstract
import PetriNet.AbstractType
import Synquid.Util
import Synquid.Pretty

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . z3env)
    getContext = gets (envContext . z3env)
    getOptimize = gets (envOptimize . z3env)

-- | create a new encoder in z3
createEncoder :: [Id] -> Id -> [FunctionCode] -> Encoder ()
createEncoder inputs ret sigs = do
    places <- gets ((:) "void" . HashMap.keys . ty2tr)
    transIds <- gets (nubOrd . concat . HashMap.elems . ty2tr)
    -- create all the type variables for encoding
    createVariables places transIds
    -- add all the constraints for the solver
    createConstraints places sigs
    -- set initial and final state for solver
    setInitialState inputs places
    setFinalState ret places

-- | set the initial state for the solver, where we have tokens only in void or inputs
-- the tokens in the other places should be zero
setInitialState :: [Id] -> [Id] -> Encoder ()
setInitialState inputs places = do
    let nonInputs = filter (`notElem` inputs) places
    let inputCounts = map (\t -> (head t, length t)) (group (sort inputs))
    let nonInputCounts = map (\t -> (t, if t == "void" then 1 else 0)) nonInputs
    let typeCounts = inputCounts ++  nonInputCounts
    -- assign tokens to each types
    mapM_ (uncurry assignToken) typeCounts
  where
    assignToken p v = do
        placeMap <- gets place2variable
        tVar <- mkZ3IntVar $ findVariable (p, 0) placeMap
        eq <- mkIntNum v >>= mkEq tVar
        modify $ \st -> st { optionalConstraints = eq : optionalConstraints st }

-- | set the final solver state, we allow only one token in the return type
-- and maybe several tokens in the "void" place
setFinalState :: Id -> [Id] -> Encoder ()
setFinalState ret places = do
    -- the return value should have only one token
    includeRet
    -- other places excluding void and ret should have nothing
    let nonOutputs = filter (ret /=) places
    mapM_ excludeOther nonOutputs
  where
    includeRet = do
        placeMap <- gets place2variable
        l <- gets loc
        retVar <- mkZ3IntVar $ findVariable (ret, l) placeMap
        assrt <- mkIntNum 1 >>= mkEq retVar
        modify $ \st -> st { finalConstraints = assrt : finalConstraints st }

    excludeOther p = do
        l <- gets loc
        placeMap <- gets place2variable
        when (p /= "void") $ do
            tVar <- mkZ3IntVar $ findVariable (p, l) placeMap
            eq <- mkIntNum 0 >>= mkEq tVar
            modify $ \st -> st { finalConstraints = eq : finalConstraints st }
            -- assert eq

getParam :: Encoder (Int, Z3.Sort)
getParam = do
    cnt <- gets counter
    boolS <- mkBoolSort
    return (cnt, boolS)

cancelConstraints :: String -> Encoder ()
cancelConstraints name = do
    (cnt, boolS) <- getParam
    cancelSym <- mkStringSymbol $ name ++ show cnt
    cancelExp <- mkConst cancelSym boolS >>= mkNot
    assert cancelExp

addAllConstraints :: Encoder ()
addAllConstraints = do
    pcons <- gets persistConstraints
    ocons <- gets optionalConstraints
    fcons <- gets finalConstraints
    bcons <- gets blockConstraints
    mapM_ assert pcons
    mapM_ assert ocons
    mapM_ assert fcons
    mapM_ assert bcons
    
nonincrementalSolve :: Encoder Z3.Result
nonincrementalSolve = do
    prev <- gets prevChecked
    when prev $ do
        toBlock <- gets block
        modify $ \st -> st { blockConstraints = toBlock : blockConstraints st}

    addAllConstraints
    check    

incrementalSolve :: Encoder Z3.Result
incrementalSolve = do
    modify $ \st -> st { counter = counter st + 1 }
    prev <- gets prevChecked
    (cnt, boolS) <- getParam
    blockSym <- mkStringSymbol $ "block" ++ show cnt
    blockE <- mkConst blockSym boolS
    blocked <- ifM (gets prevChecked)
                   (gets block >>= mkImplies blockE)
                   (mkTrue >>= mkImplies blockE)
    modify $ \st -> st { blockConstraints = blockE : blockConstraints st}
    exclusions <- gets optionalConstraints
    excludeSym <- mkStringSymbol $ "exclude" ++ show cnt
    excludeE <- mkConst excludeSym boolS
    excluded <- mkAnd exclusions >>= mkImplies excludeE
    finals <- gets finalConstraints
    finalSym <- mkStringSymbol $ "final" ++ show cnt
    finalE <- mkConst finalSym boolS
    finaled <- mkAnd finals >>= mkImplies finalE
    
    assert excluded
    assert finaled
    assert blocked

    blocks <- gets blockConstraints
    checkAssumptions (excludeE : finalE : blocks)

solveAndGetModel :: Encoder [(Id, Int)]
solveAndGetModel = do
    l <- gets loc
    incremental <- gets incrementalSolving
    res <- if incremental then incrementalSolve else nonincrementalSolve

    case res of
        Sat -> do
            -- liftIO $ print "sat"
            model <- solverGetModel
            places <- gets (HashMap.keys . ty2tr)
            selected <- mapM (checkLit model) [0..(l-1)]
            placed <- mapM (uncurry $ checkPlace model) [(p, t) | p <- places
                                                                , t <- [0..l]]
            blockTrs <- zipWithM blockTr [0..(l-1)] selected
            blockAss <- mkAnd (placed ++ blockTrs) >>= mkNot
            modify $ \st -> st { block = blockAss }
            unless incremental $ do
                currEnv <- gets z3env
                env <- liftIO $ freshEnv $ envContext currEnv
                modify $ \st -> st { z3env = env }
            selectedNames <- getTrNames selected
            cancelConstraints "exclude"
            cancelConstraints "final"
            return (zip selectedNames [0,1..])
        Unsat -> do
            rets <- gets returnTyps
            unless incremental $ do
                currEnv <- gets z3env
                env <- liftIO $ freshEnv $ envContext currEnv
                modify $ \st -> st { z3env = env }
            if length rets == 1
              then do
                when incremental $ do
                    cancelConstraints "exclude"
                    cancelConstraints "final"
                    mapM_ (mkNot >=> assert) blocks
                return []
              else do
                -- liftIO $ print "unsat for change goal"
                -- try a more general return type
                t2tr <- gets ty2tr
                when incremental $ cancelConstraints "final"
                modify $ \st -> st { finalConstraints = []
                                   , returnTyps = tail rets 
                                   , prevChecked = False }
                setFinalState (rets !! 1) (HashMap.keys t2tr)
                solveAndGetModel
        Undef -> return []
  where
    getTrNames selected = do
        transMap <- gets id2transition
        let transNames = map (\id -> findVariable (fromIntegral id) transMap) selected
        return transNames

    checkPlace model p t = do
        placeMap <- gets place2variable
        pVar <- mkZ3IntVar (findVariable (p, t) placeMap)
        maybeInt <- evalInt model pVar
        case maybeInt of
          Just i -> mkIntNum i >>= mkEq pVar
          Nothing -> error $ "cannot eval the variable" ++ show (p, t)

    checkLit model t = do
        tsMap <- gets time2variable
        tsVar <- mkZ3IntVar (findVariable t tsMap)
        bMay <- evalInt model tsVar
        case bMay of
            Just b -> return b
            Nothing -> error $ "cannot eval the variable" ++ show t

    blockTr t tr = do
        tsMap <- gets time2variable
        tsVar <- mkZ3IntVar (findVariable t tsMap)
        mkIntNum tr >>= mkEq tsVar

encoderInit :: Int -> HashMap Id [Id] -> [Id] -> [Id] -> [FunctionCode] -> HashMap Id [Id] -> Bool -> IO EncodeState
encoderInit loc hoArgs inputs rets sigs t2tr incr = do
    z3Env <- initialZ3Env
    false <- Z3.mkFalse (envContext z3Env)
    let initialState = EncodeState z3Env 0 false loc 0 1 HashMap.empty HashMap.empty HashMap.empty HashMap.empty hoArgs t2tr False incr [] rets [] [] [] []
    execStateT (createEncoder inputs (head rets) sigs) initialState

encoderSolve :: EncodeState -> IO ([(Id, Int)], EncodeState)
encoderSolve = runStateT solveAndGetModel

encoderInc :: [FunctionCode] -> [Id] -> [Id] -> Encoder ()
encoderInc sigs inputs rets = do
    modify $ \st -> st { loc = loc st + 1
                       , returnTyps = rets
                       , optionalConstraints = []
                       , blockConstraints = []
                       , finalConstraints = [] }
    places <- gets ((:) "void" . HashMap.keys . ty2tr)
    transitions <- gets (nubOrd . concat . HashMap.elems . ty2tr)
    l <- gets loc

    -- add new place, transition and timestamp variables
    -- TODO change addPlaceVar to include time stamp as parameters
    mapM_ (uncurry addPlaceVar) [(a, l) | a <- places]
    addTimestampVar (l - 1)

    let allTrans = [(l - 1, tr) | tr <- sigs ]

    -- all places have non-negative number of tokens
    nonnegativeTokens places

    -- refine the postcondition constraints
    mapM_ (uncurry fireTransitions) allTrans

    -- disable transitions at the new timestamp
    toRemove <- gets disabledTrans
    disableTransitions toRemove (l-1)

    -- save the current state and add changeable constraints
    transitionRng

    mapM_ (uncurry noTransitionTokens) [(t, p) | p <- places, t <- [0..(l-1)]]

    -- refine the must firers
    mustFireTransitions

    -- set new initial and final state
    setInitialState inputs places
    
    setFinalState (head rets) places

encoderRefine :: SplitInfo -> HashMap Id [Id] -> [Id] -> [Id] -> [FunctionCode] -> HashMap Id [Id] -> Encoder ()
encoderRefine info musters inputs rets sigs t2tr = do
    {- update the abstraction level -}
    modify $ \st -> st { ty2tr = t2tr 
                       , mustFirers = musters
                       , disabledTrans = disabledTrans st ++ removedTrans info
                       , returnTyps = rets
                       , optionalConstraints = []
                       , finalConstraints = []
                       }

    {- operation on places -}
    l <- gets loc
    let newPlaceIds = map show (newPlaces info)
    let newTransIds = newTrans info
    let currPlaces = HashMap.keys t2tr
    let newSigs = filter ((`elem` newTransIds) . funName) sigs
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- newSigs ]

    -- add new place, transition and timestamp variables
    mapM_ (uncurry addPlaceVar) [(a, i) | a <- newPlaceIds, i <- [0..l]]
    addTransitionVar newTransIds    

    -- all places have non-negative number of tokens
    nonnegativeTokens newPlaceIds

    -- refine the postcondition constraints
    mapM_ (uncurry fireTransitions) allTrans

    -- disable splitted transitions
    mapM_ (disableTransitions (removedTrans info)) [0..(l-1)]

    transitionRng

    mapM_ (uncurry noTransitionTokens) [(t, p) | p <- currPlaces, t <- [0..(l-1)]]

    -- refine the must firers
    mustFireTransitions

    -- set new initial and final state
    setInitialState inputs currPlaces
    
    setFinalState (head rets) currPlaces

disableTransitions :: [Id] -> Int -> Encoder ()
disableTransitions trs t = mapM_ disableTrAt trs
  where
    disableTrAt tr = do
        transMap <- gets transition2id
        tsMap <- gets time2variable
        trVar <- mkIntNum (findVariable tr transMap)
        tsVar <- mkZ3IntVar (findVariable t tsMap)
        eq <- mkEq tsVar trVar >>= mkNot
        modify $ \st -> st { persistConstraints = eq : persistConstraints st }
        assert eq

-- | add variables for each place:
-- 1) an integer variable for number of default tokens
-- 2) an integer variable for number of colored tokens
-- 3) an integer variable for color index
-- in this design, we allow only one color existing in the petri net
-- so there is no nested higher order functions
addPlaceVar ::  Id -> Int -> Encoder ()
addPlaceVar p t = do
    st <- get
    let placeVar = Variable (variableNb st) (p ++ "_" ++ show t) t 0 VarPlace
    let p2v = HashMap.insert (p, t) placeVar $ place2variable st
    unless (HashMap.member (p, t) (place2variable st))
            (put $ st { place2variable = p2v
                        , variableNb = variableNb st + 1
                        })

-- | add transition mapping from (tr, lv) to integer id
-- 1) an integer variable for each transition
-- 2) an fresh integer variable for each higher-order argument
addTransitionVar :: [Id] -> Encoder ()
addTransitionVar = mapM_ addTransitionVarFor
  where
    addTransitionVarFor tr = do
        st <- get
        let tid = transitionNb st
        unless (HashMap.member tr (transition2id st))
               (put $ st { transitionNb = 1 + transitionNb st
                         , transition2id = HashMap.insert tr tid $ transition2id st
                         , id2transition = HashMap.insert tid tr $ id2transition st
                         })

addTimestampVar :: Int -> Encoder ()
addTimestampVar t = do
    st <- get
    let tsVar = Variable (variableNb st) ("ts_" ++ show t) t 0 VarTimestamp
    unless (HashMap.member t (time2variable st))
           (put $ st { time2variable = HashMap.insert t tsVar $ time2variable st
                     , variableNb = variableNb st + 1
                     })

-- | map each place and transition to a variable in z3
createVariables :: [Id] -> [Id] -> Encoder ()
createVariables places transitions = do
    l <- gets loc
    -- add place variables
    mapM_ (uncurry addPlaceVar) [(a, i) | a <- places, i <- [0..l]]
    -- add transition mapping
    addTransitionVar transitions
    -- add timestamp variables
    mapM_ addTimestampVar [0..(l-1)]

createConstraints :: [Id] -> [FunctionCode] -> Encoder ()
createConstraints places transitions = do
    -- prepare constraint parameters
    liftIO $ print places
    l <- gets loc
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- transitions]
    let allPlaces = [(t, p) | t <- [0..(l-1)], p <- places]

    nonnegativeTokens places

    mapM_ (uncurry fireTransitions) allTrans

    transitionRng

    mapM_ (uncurry noTransitionTokens) allPlaces

    mustFireTransitions


mkZ3BoolVar ::  Variable -> Encoder AST
mkZ3BoolVar var = do
    varSymbol <- mkIntSymbol (varId var)
    boolS <- mkBoolSort
    mkConst varSymbol boolS

mkZ3IntVar :: Variable -> Encoder AST
mkZ3IntVar var = do
    varSymbol <- mkIntSymbol (varId var)
    intS <- mkIntSort
    mkConst varSymbol intS

findVariable :: (Eq k, Hashable k, Show k) => k -> HashMap k v -> v
findVariable k m = fromMaybe (error $ "cannot find variable for " ++ show k)
                             (HashMap.lookup k m)

nonnegativeTokens :: [Id] -> Encoder ()
nonnegativeTokens places = do
    l <- gets loc
    mapM_ (uncurry nonnegAt) [(p, t) | p <- places, t <- [0..l]]
  where
    nonnegAt p t = do
        placeMap <- gets place2variable
        let pVar = findVariable (p, t) placeMap
        pZ3Var <- mkZ3IntVar pVar
        zero <- mkIntNum 0
        geZero <- mkGe pZ3Var zero
        modify $ \st -> st { persistConstraints = geZero : persistConstraints st }
        assert geZero

-- | at each timestamp, only one transition can be fired, we restrict the
-- fired transition id range here
transitionRng :: Encoder ()
transitionRng = do
    l <- gets loc
    mapM_ fireAt [0..(l-1)]
  where
    fireAt t = do
        tsMap <- gets time2variable
        transMax <- gets transitionNb
        let tsVar = findVariable t tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        start <- mkIntNum 0
        geStart <- mkGe tsZ3Var start
        end <- mkIntNum transMax
        ltEnd <- mkLt tsZ3Var end
        modify $ \st -> st { optionalConstraints = ltEnd : geStart : optionalConstraints st }

-- | if this place has no connected transition fired,
-- it has the same # of tokens
noTransitionTokens :: Int -> Id -> Encoder ()
noTransitionTokens t p = do
    trans <- gets transition2id
    t2tr <- gets ty2tr
    let transitions = map (`findVariable` trans) (HashMap.lookupDefault [] p t2tr)
    noFireLvs <- noFireAt transitions p t
    noFire <- mkOr noFireLvs >>= mkNot
    placeMap <- gets place2variable
    curr <- mkZ3IntVar $ findVariable (p, t) placeMap
    next <- mkZ3IntVar $ findVariable (p, t + 1) placeMap
    tokenSame <- mkEq curr next
    noChange <- mkImplies noFire tokenSame
    modify $ \st -> st { optionalConstraints = noChange : optionalConstraints st }
  where
    noFireAt transitions p t = do
        idVars <- mapM mkIntNum transitions
        tsMap <- gets time2variable
        let tsVar = findVariable t tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        mapM (mkEq tsZ3Var) idVars

fireTransitions :: Int -> FunctionCode -> Encoder ()
fireTransitions t (FunctionCode name [] params rets) = do
    transMap <- gets transition2id
    placeMap <- gets place2variable
    tsMap <- gets time2variable
    -- accumulate counting for parameters and return types
    let tid = findVariable name transMap
    let pcnt = if null params then [("void", 1)] 
                              else map (\l -> (head l, length l)) (group (sort params))
    let pmap = HashMap.fromList pcnt
    let rmap = foldl' (\acc t -> HashMap.insertWith (+) t (-1) acc) pmap rets
    let rcnt = HashMap.toList rmap
    let tsVar = findVariable t tsMap
    tsZ3Var <- mkZ3IntVar tsVar
    trVar <- mkIntNum tid
    changes <- mapM (mkChange t) rcnt
    -- all parameter tokens must have the same color
    -- if one place has the id of p, then p + 1 indicates its colorful token number
    -- p + 2 indicates its color index
    let places = map fst pcnt
    fire <- mkEq tsZ3Var trVar
    enoughTokens <- mapM getSatisfiedPlace pcnt
    postCond <- mkAnd (enoughTokens ++ changes)
    tokenChange <- mkImplies fire postCond
    modify $ \st -> st { persistConstraints = tokenChange : persistConstraints st }
    assert tokenChange
  where
    mkChange t (p, diff) = do
        let d = if p == "void" then 0 else -diff
        placeMap <- gets place2variable
        before <- mkZ3IntVar $ findVariable (p, t) placeMap
        after <- mkZ3IntVar $ findVariable (p, t + 1) placeMap
        diffw <- mkIntNum d
        mkAdd [before, diffw] >>= mkEq after

    getSatisfiedPlace (p, cnt) = do
        w <- mkIntNum cnt
        placeMap <- gets place2variable
        pVar <- mkZ3IntVar (findVariable (p, t) placeMap)
        mkGe pVar w

mustFireTransitions ::  Encoder ()
mustFireTransitions = do
    must <- gets mustFirers
    mapM_ fireTransitionFor (HashMap.toList must)
  where
    nameInMust must name = foldr ((||) . flip isInfixOf name) False must
    fireTransition tid = do
        l <- gets loc
        tsMap <- gets time2variable
        trId <- mkIntNum tid
        tsVars <- mapM (\t -> mkZ3IntVar (findVariable t tsMap)) [0..(l-1)]
        mapM (mkEq trId) tsVars

    fireTransitionFor (id, tids) = do
        transitions <- gets transition2id
        let mustTrans = HashMap.filterWithKey (\k _ -> nameInMust tids k) transitions
        fires <- mapM fireTransition mustTrans
        toFire <- mkOr (concat fires)
        modify $ \st -> st { optionalConstraints = toFire : optionalConstraints st }
