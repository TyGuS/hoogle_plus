{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Set (Set)
import qualified Data.Set as Set
import Z3.Monad hiding(Z3Env, newEnv)
import qualified Z3.Base as Z3
import Control.Monad.State
import System.CPUTime
import Text.Printf
import Data.Text (pack, unpack, replace)
import System.IO
import System.Process

import Types.Common
import Types.Encoder
import Types.Abstract
import PetriNet.AbstractType
import PetriNet.Util
import Synquid.Util
import Synquid.Pretty

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . z3env)
    getContext = gets (envContext . z3env)

-- | create a new encoder in z3
createEncoder :: [AbstractSkeleton] -> AbstractSkeleton -> [FunctionCode] -> Encoder ()
createEncoder inputs ret sigs = do
    places <- gets (HashMap.keys . ty2tr)
    transIds <- gets (Set.toList . Set.unions . HashMap.elems . ty2tr)
    -- create all the type variables for encoding
    -- liftIO $ putStrLn "creating variables"
    createVariables places transIds
    -- add all the constraints for the solver
    -- liftIO $ putStrLn "creating constraints"
    createConstraints places sigs
    -- set initial and final state for solver
    setInitialState inputs places
    setFinalState ret places

-- | set the initial state for the solver, where we have tokens only in void or inputs
-- the tokens in the other places should be zero
setInitialState :: [AbstractSkeleton] -> [AbstractSkeleton] -> Encoder ()
setInitialState inputs places = do
    let nonInputs = filter (`notElem` inputs) places
    let inputCounts = map (\t -> (head t, length t)) (group (sort inputs))
    let nonInputCounts = map (, 0) nonInputs
    mapM_ (uncurry assignToken) nonInputCounts
    enforceArg <- gets useArguments
    when enforceArg $ mapM_ (uncurry assignInput) inputCounts
  where
    assignToken p v = do
        placeMap <- gets place2variable
        let tVar = findVariable "place2variable" (p, 0) placeMap
        eq <- mkIntNum v >>= mkEq tVar
        modify $ \st -> st { optionalConstraints = eq : optionalConstraints st }

    assignInput p v = do
        placeMap <- gets place2variable
        let tVar = findVariable "place2variable" (p, 0) placeMap
        noClone <- gets disableClones
        ge <- mkIntNum v >>= if noClone then mkGe tVar else mkEq tVar
        modify $ \st -> st { optionalConstraints = ge : optionalConstraints st }

-- | set the final solver state, we allow only one token in the return type
-- and maybe several tokens in the "void" place
setFinalState :: AbstractSkeleton -> [AbstractSkeleton] -> Encoder ()
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
        let retVar = findVariable "final place2variable" (ret, l) placeMap
        assrt <- mkIntNum 1 >>= mkEq retVar
        modify $ \st -> st { finalConstraints = assrt : finalConstraints st }

    excludeOther p = do
        l <- gets loc
        placeMap <- gets place2variable
        let tVar = findVariable "final place2variable" (p, l) placeMap
        eq <- mkIntNum 0 >>= mkEq tVar
        modify $ \st -> st { finalConstraints = eq : finalConstraints st }

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
        modify $ \st -> st { blockConstraints = toBlock : blockConstraints st
                            , prevChecked = False }

    addAllConstraints
    str <- solverToString
    liftIO $ putStrLn str
    -- liftIO $ writeFile "constraint.z3" str
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
    modify $ \st -> st { blockConstraints = blockE : blockConstraints st
                        , prevChecked = False }
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

solveAndGetModel :: Encoder [Id]
solveAndGetModel = do
    l <- gets loc
    incremental <- gets incrementalSolving
    res <- if incremental then incrementalSolve else nonincrementalSolve

    case res of
        Sat -> do
            liftIO $ putStrLn "sat"
            model <- solverGetModel
            places <- gets (HashMap.keys . ty2tr)
            -- evaluate what transitions are fired
            selected <- mapM (checkLit model) [0..(l-1)]
            placed <- mapM (uncurry $ checkPlace model) [(p, t) | p <- places
                                                                , t <- [0..l]]
            blockTrs <- zipWithM blockTr [0..(l-1)] selected
            blockAss <- mkAnd (placed ++ blockTrs) >>= mkNot
            modify $ \s -> s { block = blockAss }
            unless incremental solverReset
            selectedNames <- getTrNames selected
            when incremental $ do
                cancelConstraints "exclude"
                cancelConstraints "final"
            return selectedNames
        Unsat -> do
            liftIO $ putStrLn "unsat"
            rets <- gets returnTyps
            unless incremental solverReset
            if length rets == 1
              then do
                -- liftIO $ print "unsat for increase length"
                when incremental $ do
                    cancelConstraints "exclude"
                    cancelConstraints "final"
                    blocks <- gets blockConstraints
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
        let transNames = map (\id -> findVariable "id2transition" (fromIntegral id) transMap) selected
        return transNames

    checkPlace model p t = do
        placeMap <- gets place2variable
        let pVar = findVariable "placemap" (p, t) placeMap
        maybeInt <- evalInt model pVar
        case maybeInt of
          Just i -> mkIntNum i >>= mkEq pVar
          Nothing -> error $ "cannot eval the variable" ++ show (p, t)

    checkLit model t = do
        tsMap <- gets time2variable
        let tsVar = findVariable "time2variable" t tsMap
        bMay <- evalInt model tsVar
        case bMay of
            Just b -> return b
            Nothing -> error $ "cannot eval the variable" ++ show t

    blockTr t tr = do
        tsMap <- gets time2variable
        let tsVar = findVariable "time2variable" t tsMap
        mkIntNum tr >>= mkEq tsVar

encoderInit :: Int
            -> HashMap Id [Id]
            -> [AbstractSkeleton]
            -> [AbstractSkeleton]
            -> [FunctionCode]
            -> HashMap AbstractSkeleton (Set Id)
            -> Bool
            -> Bool
            -> Bool
            -> IO EncodeState
encoderInit len hoArgs inputs rets sigs t2tr incr rele noClone = do
    z3Env <- initialZ3Env
    false <- Z3.mkFalse (envContext z3Env)
    let initialState = emptyEncodeState {
          z3env = z3Env
        , loc = len
        , mustFirers = hoArgs
        , ty2tr = t2tr
        , incrementalSolving = incr
        , returnTyps = rets
        , useArguments = not rele
        , disableClones = noClone
        }
    execStateT (createEncoder inputs (head rets) sigs) initialState

encoderSolve :: EncodeState -> IO ([Id], EncodeState)
encoderSolve = runStateT solveAndGetModel

-- optimize the optional constraints here:
-- we only need to change the must firers and noTransitionTokens and final states
encoderInc :: [FunctionCode] -> [AbstractSkeleton] -> [AbstractSkeleton] -> Encoder ()
encoderInc sigs inputs rets = do
    modify $ \st -> st { loc = loc st + 1
                       , returnTyps = rets
                       , optionalConstraints = []
                       , blockConstraints = []
                       , finalConstraints = []
                       , prevChecked = False }
    places <- gets (HashMap.keys . ty2tr)
    transitions <- gets (Set.toList . Set.unions . HashMap.elems . ty2tr)
    l <- gets loc

    -- add new place, transition and timestamp variables
    mapM_ (uncurry addPlaceVar) [(a, l) | a <- places ++ inputs ++ rets]
    addTimestampVar (l - 1)

    let allTransitions = [(l - 1, tr) | tr <- sigs ]

    -- all places have non-negative number of tokens
    -- liftIO $ print "inc - nonnegative"
    nonnegativeTokens places

    -- disable transitions at the new timestamp
    -- liftIO $ print "inc - disable"
    toRemove <- gets disabledTrans
    disableTransitions toRemove (l-1)

    -- refine the postcondition constraints
    -- liftIO $ print "inc - fire"
    mapM_ (uncurry fireTransitions) allTransitions

    -- save the current state and add changeable constraints
    -- liftIO $ print "inc - rng"
    transitionRng

    -- liftIO $ print "inc - no transition"
    mapM_ (uncurry noTransitionTokens) [(t, p) | p <- places, t <- [0..(l-1)]]

    -- refine the must firers
    -- liftIO $ print "inc - must"
    mustFireTransitions

    -- set new initial and final state
    setInitialState inputs places

    setFinalState (head rets) places

encoderRefine :: SplitInfo -> HashMap Id [Id] -> [AbstractSkeleton] -> [AbstractSkeleton] -> [FunctionCode] -> HashMap AbstractSkeleton (Set Id) -> Encoder ()
encoderRefine info musters inputs rets newSigs t2tr = do
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
    let newPlaceIds = newPlaces info
    let newTransIds = newTrans info
    let currPlaces = HashMap.keys t2tr
    -- let newSigs = filter ((`elem` newTransIds) . funName) sigs
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- newSigs ]
    -- liftIO $ print newSigs
    -- add new place, transition and timestamp variables
    -- liftIO $ print newPlaceIds
    mapM_ (uncurry addPlaceVar) [(a, i) | a <- newPlaceIds, i <- [0..l]]
    -- liftIO $ print newTransIds
    addTransitionVar newTransIds

    -- all places have non-negative number of tokens
    -- liftIO $ print "refine - nonnegative"
    nonnegativeTokens newPlaceIds

    -- refine the postcondition constraints
    -- liftIO $ print "refine - fire"
    mapM_ (uncurry fireTransitions) allTrans

    -- disable splitted transitions
    -- liftIO $ print "refine - disable"
    mapM_ (disableTransitions (removedTrans info)) [0..(l-1)]

    -- liftIO $ print "refine - rng"
    transitionRng

    -- liftIO $ print "refine - no transitions"
    mapM_ (uncurry noTransitionTokens) [(t, p) | p <- currPlaces, t <- [0..(l-1)]]

    -- refine the must firers
    -- liftIO $ print "refine - must"
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
        let trVar = findVariable "transition2id" tr transMap
        let tsVar = findVariable "time2variable" t tsMap
        eq <- mkEq tsVar trVar >>= mkNot
        modify $ \st -> st { persistConstraints = eq : persistConstraints st }
        incremental <- gets incrementalSolving
        when incremental $ assert eq

-- | add variables for each place
addPlaceVar ::  AbstractSkeleton -> Int -> Encoder ()
addPlaceVar p t = do
    st <- get
    placeVar <- mkZ3IntVar $ variableNb st
    let p2v = HashMap.insert (p, t) placeVar $ place2variable st
    unless (HashMap.member (p, t) (place2variable st))
            (put $ st { place2variable = p2v
                      , variableNb = variableNb st + 1
                      })

-- | add transition mapping from (tr, lv) to integer id
-- an integer variable for each transition
addTransitionVar :: [Id] -> Encoder ()
addTransitionVar = mapM_ addTransitionVarFor
  where
    addTransitionVarFor tr = do
        st <- get
        let tid = transitionNb st
        trVar <- mkIntNum tid
        unless (HashMap.member tr (transition2id st))
               (put $ st { transitionNb = 1 + transitionNb st
                         , transition2id = HashMap.insert tr trVar $ transition2id st
                         , id2transition = HashMap.insert tid tr $ id2transition st
                         })

addTimestampVar :: Int -> Encoder ()
addTimestampVar t = do
    st <- get
    tsVar <- mkZ3IntVar $ variableNb st
    unless (HashMap.member t (time2variable st))
           (put $ st { time2variable = HashMap.insert t tsVar $ time2variable st
                     , variableNb = variableNb st + 1
                     })

-- | map each place and transition to a variable in z3
createVariables :: [AbstractSkeleton] -> [Id] -> Encoder ()
createVariables places transitions = do
    l <- gets loc
    -- add place variables
    -- liftIO $ putStrLn "adding place variables"
    mapM_ (uncurry addPlaceVar) [(a, i) | a <- places, i <- [0..l]]
    -- add transition mapping
    -- liftIO $ putStrLn "adding transitions variables"
    addTransitionVar transitions
    -- add timestamp variables
    -- liftIO $ putStrLn "adding time variables"
    mapM_ addTimestampVar [0..(l-1)]

createConstraints :: [AbstractSkeleton] -> [FunctionCode] -> Encoder ()
createConstraints places transitions = do
    -- prepare constraint parameters
    -- liftIO $ print places
    l <- gets loc
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- transitions]
    let allPlaces = [(t, p) | t <- [0..(l-1)], p <- places]

    -- liftIO $ putStrLn "adding more constraints"
    nonnegativeTokens places

    mapM_ (uncurry fireTransitions) allTrans

    transitionRng

    mapM_ (uncurry noTransitionTokens) allPlaces

    mustFireTransitions

mkZ3IntVar :: Int -> Encoder AST
mkZ3IntVar var = do
    varSymbol <- mkIntSymbol var
    intS <- mkIntSort
    mkConst varSymbol intS

findVariable :: (Eq k, Hashable k, Show k) => String -> k -> HashMap k v -> v
findVariable blame k m = fromMaybe (error $ "cannot find in " ++ blame ++ " variable for " ++ show k)
                             (HashMap.lookup k m)

nonnegativeTokens :: [AbstractSkeleton] -> Encoder ()
nonnegativeTokens places = do
    l <- gets loc
    mapM_ (uncurry nonnegAt) [(p, t) | p <- places, t <- [0..l]]
  where
    nonnegAt p t = do
        placeMap <- gets place2variable
        let pVar = findVariable "placemap" (p, t) placeMap
        zero <- mkIntNum 0
        geZero <- mkGe pVar zero
        modify $ \st -> st { persistConstraints = geZero : persistConstraints st }
        incremental <- gets incrementalSolving
        when incremental $ assert geZero

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
        let tsVar = findVariable "time2variable" t tsMap
        start <- mkIntNum 0
        geStart <- mkGe tsVar start
        end <- mkIntNum transMax
        ltEnd <- mkLt tsVar end
        modify $ \st -> st { optionalConstraints = ltEnd : geStart : optionalConstraints st }

-- | if this place has no connected transition fired,
-- it has the same # of tokens
noTransitionTokens :: Int -> AbstractSkeleton -> Encoder ()
noTransitionTokens t p = do
    trans <- gets transition2id
    t2tr <- gets ty2tr
    let transSet = Set.toList $ HashMap.lookupDefault Set.empty p t2tr
    let transitions = map (\x -> findVariable "transition2id" x trans) transSet
    noFireLvs <- noFireAt transitions t
    noFire <- mkOr noFireLvs >>= mkNot
    placeMap <- gets place2variable
    let curr = findVariable "placemap" (p, t) placeMap
    let next = findVariable "placemap" (p, t + 1) placeMap
    tokenSame <- mkEq curr next
    noChange <- mkImplies noFire tokenSame
    modify $ \st -> st { optionalConstraints = noChange : optionalConstraints st }
  where
    noFireAt transitions t = do
        tsMap <- gets time2variable
        let tsVar = findVariable "time2variable" t tsMap
        mapM (mkEq tsVar) transitions

fireTransitions :: Int -> FunctionCode -> Encoder ()
fireTransitions t (FunctionCode name [] params rets) = do
    transMap <- gets transition2id
    placeMap <- gets place2variable
    tsMap <- gets time2variable

    -- accumulate counting for parameters and return types
    let pcnt = map (\l -> (head l, length l)) (group (sort params))
    let pmap = HashMap.fromList pcnt
    let rmap = foldl' (\acc t -> HashMap.insertWith (+) t (-1) acc) pmap rets
    let rcnt = HashMap.toList rmap
    changes <- mapM (mkChange t) rcnt

    let tsVar = findVariable "time2variable" t tsMap
    let trVar = findVariable "transition2id" name transMap
    fire <- mkEq tsVar trVar
    enoughTokens <- mapM getSatisfiedPlace pcnt
    postCond <- mkAnd (enoughTokens ++ changes)
    tokenChange <- mkImplies fire postCond
    modify $ \st -> st { persistConstraints = tokenChange : persistConstraints st }
    incremental <- gets incrementalSolving
    when incremental $ assert tokenChange
  where
    mkChange t (p, diff) = do
        let d = -diff
        placeMap <- gets place2variable
        let before = findVariable "placemap" (p, t) placeMap
        let after = findVariable "placemap" (p, t + 1) placeMap
        diffw <- mkIntNum d
        mkAdd [before, diffw] >>= mkEq after

    getSatisfiedPlace (p, cnt) = do
        w <- mkIntNum cnt
        placeMap <- gets place2variable
        let pVar = findVariable "placemap" (p, t) placeMap
        mkGe pVar w
fireTransitions t fc = error $ "unhandled " ++ show fc

mustFireTransitions ::  Encoder ()
mustFireTransitions = do
    must <- gets mustFirers
    mapM_ fireTransitionFor (HashMap.toList must)
  where
    nameInMust must name = foldr ((||) . flip isInfixOf name) False must
    fireTransition tid = do
        l <- gets loc
        tsMap <- gets time2variable
        let tsVars = map (\t -> findVariable "time2variable" t tsMap) [0..(l-1)]
        mapM (mkEq tid) tsVars

    fireTransitionFor (_, tids) = do
        transitions <- gets transition2id
        let mustTrans = HashMap.filterWithKey (\k _ -> nameInMust tids k) transitions
        fires <- mapM fireTransition mustTrans
        toFire <- mkOr (concat fires)
        modify $ \st -> st { optionalConstraints = toFire : optionalConstraints st }
