{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module PetriNet.PNEncoder(
     encoderInit
    , encoderSolve
    , encoderRefine
    , encoderInc
    ) where

import Data.Maybe ( fromMaybe )
import Data.List
import Data.Hashable ( Hashable )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Z3.Monad hiding(Z3Env, newEnv)
import qualified Z3.Base as Z3
import Control.Monad.State
import Data.Text (pack, unpack, replace)
import Control.Lens

import Types.Common
import Types.Encoder
import Types.Experiments
import Types.Type
import Utility.Utils

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . _z3env)
    getContext = gets (envContext . _z3env)

-- | create a new encoder in z3
createEncoder :: [TypeSkeleton] -> TypeSkeleton -> [EncodedFunction] -> Encoder ()
createEncoder inputs ret sigs = do
    ty2tr <- gets $ view (variables . type2transition)
    let places = Map.keys ty2tr
    let transIds = Set.toList $ Set.unions $ Map.elems ty2tr
    -- create all the type variables for encoding
    createVariables places transIds
    -- add all the constraints for the solver
    createConstraints places sigs
    -- set initial and final state for solver
    setInitialState inputs places
    setFinalState ret places

-- | set the initial state for the solver, where we have tokens only in void or inputs
-- the tokens in the other places should be zero
setInitialState :: [TypeSkeleton] -> [TypeSkeleton] -> Encoder ()
setInitialState inputs places = do
    let nonInputs = filter (`notElem` inputs) places
    let inputCounts = map (\t -> (head t, length t)) (group (sort inputs))
    let nonInputCounts = map (, 0) nonInputs
    mapM_ (uncurry assignToken) nonInputCounts
    disgardArg <- gets $ view (encSearchParams . disableRelevancy)
    unless disgardArg $ mapM_ (uncurry assignInput) inputCounts
  where
    assignToken p v = do
        placeMap <- gets $ view (variables . place2variable)
        let tVar = lookupWithError "place2variable" (p, 0) placeMap
        eq <- mkIntNum v >>= mkEq tVar
        modify $ over (constraints . optionalConstraints) (eq :)

    assignInput p v = do
        placeMap <- gets $ view (variables . place2variable)
        let tVar = lookupWithError "place2variable" (p, 0) placeMap
        noClone <- gets $ view (encSearchParams . disableCopy)
        ge <- mkIntNum v >>= if noClone then mkGe tVar else mkEq tVar
        modify $ over (constraints . optionalConstraints) (ge :)

-- | set the final solver state, we allow only one token in the return type
-- and maybe several tokens in the "void" place
setFinalState :: TypeSkeleton -> [TypeSkeleton] -> Encoder ()
setFinalState ret places = do
    -- the return value should have only one token
    includeRet
    -- other places excluding void and ret should have nothing
    let nonOutputs = filter (ret /=) places
    mapM_ excludeOther nonOutputs
  where
    includeRet = do
        placeMap <- gets $ view (variables . place2variable)
        l <- gets $ view pathLength
        let retVar = lookupWithError "place2variable" (ret, l) placeMap
        assrt <- mkIntNum 1 >>= mkEq retVar
        modify $ over (constraints . finalConstraints) (assrt :)

    excludeOther p = do
        l <- gets $ view pathLength
        placeMap <- gets $ view (variables . place2variable)
        let tVar = lookupWithError "place2variable" (p, l) placeMap
        eq <- mkIntNum 0 >>= mkEq tVar
        modify $ over (constraints . finalConstraints) (eq :)

addAllConstraints :: Encoder ()
addAllConstraints = do
    Constraints p o f b <- gets $ view constraints
    mapM_ assert p
    mapM_ assert o
    mapM_ assert f
    mapM_ assert b

nonincrementalSolve :: Encoder Z3.Result
nonincrementalSolve = do
    prev <- gets $ view prevChecked
    when prev $ do
        toBlock <- gets $ view block
        modify $ over (constraints . blockConstraints) (toBlock :)
        modify $ set prevChecked False

    addAllConstraints
    check

solveAndGetModel :: Encoder [Id]
solveAndGetModel = do
    l <- gets $ view pathLength
    res <- nonincrementalSolve
    mode <- gets $ view currentMode

    case res of
        Sat -> do
            model <- solverGetModel
            ty2tr <- gets $ view (variables . type2transition)
            let places = Map.keys ty2tr
            -- evaluate what transitions are fired
            selected <- mapM (checkLit model) [0..(l-1)]
            placed <- mapM (uncurry $ checkPlace model) [(p, t) | p <- places
                                                                , t <- [0..l]]
            blockTrs <- zipWithM blockTr [0..(l-1)] selected
            blockAss <- mkAnd (placed ++ blockTrs) >>= mkNot
            modify $ set block blockAss
            when (mode /= Enumeration) solverReset
            getTrNames selected
        Unsat -> do
            rets <- gets $ view returnTyps
            when (mode /= Enumeration) solverReset
            if length rets == 1
              then do
                blocks <- gets $ view (constraints . blockConstraints)
                mapM_ (mkNot >=> assert) blocks
                return []
              else do
                -- liftIO $ print "unsat for change goal"
                -- try a more general return type
                t2tr <- gets $ view (variables . type2transition)
                modify $ set (constraints . finalConstraints) []
                modify $ set returnTyps (tail rets)
                modify $ set prevChecked False
                setFinalState (rets !! 1) (Map.keys t2tr)
                solveAndGetModel
        Undef -> return []
  where
    getTrNames selected = do
        transMap <- gets $ view (variables . id2transition)
        let transNames = map (\id -> lookupWithError "id2transition" (fromIntegral id) transMap) selected
        return transNames

    checkPlace model p t = do
        placeMap <- gets $ view (variables . place2variable)
        let pVar = lookupWithError "placemap" (p, t) placeMap
        maybeInt <- evalInt model pVar
        case maybeInt of
          Just i -> mkIntNum i >>= mkEq pVar
          Nothing -> error $ "cannot eval the variable" ++ show (p, t)

    checkLit model t = do
        tsMap <- gets $ view (variables . time2variable)
        let tsVar = lookupWithError "time2variable" t tsMap
        bMay <- evalInt model tsVar
        case bMay of
            Just b -> return b
            Nothing -> error $ "cannot eval the variable" ++ show t

    blockTr t tr = do
        tsMap <- gets $ view (variables . time2variable)
        let tsVar = lookupWithError "time2variable" t tsMap
        mkIntNum tr >>= mkEq tsVar

encoderInit :: EncodeState
            -> Int
            -> [TypeSkeleton]
            -> [TypeSkeleton]
            -> [EncodedFunction]
            -> IO EncodeState
encoderInit encoderState len inputs rets sigs = do
    z3Env <- initialZ3Env
    false <- Z3.mkFalse (envContext z3Env)
    execStateT (do
        modify $ set z3env z3Env
        modify $ set pathLength len
        modify $ set returnTyps rets
        createEncoder inputs (head rets) sigs) encoderState

encoderSolve :: EncodeState -> IO ([Id], EncodeState)
encoderSolve = runStateT solveAndGetModel

-- optimize the optional constraints here:
-- we only need to change the must firers and noTransitionTokens and final states
encoderInc :: [EncodedFunction] -> [TypeSkeleton] -> [TypeSkeleton] -> Encoder ()
encoderInc sigs inputs rets = do
    modify $ over pathLength (+ 1)
    modify $ set returnTyps rets
    persists <- gets $ view (constraints . persistConstraints)
    modify $ set constraints ( emptyConstraints { _persistConstraints = persists })

    ty2tr <- gets $ view (variables . type2transition)
    l <- gets $ view pathLength
    let places = Map.keys ty2tr
    let transitions = Set.toList $ Set.unions $ Map.elems ty2tr

    -- add new place, transition and timestamp variables
    mapM_ (uncurry addPlaceVar) [(a, l) | a <- places]
    addTimestampVar (l - 1)

    let allTransitions = [(l - 1, tr) | tr <- sigs ]

    -- all places have non-negative number of tokens
    nonnegativeTokens places

    -- disable transitions at the new timestamp
    toRemove <- gets $ view disabledTrans
    disableTransitions toRemove (l - 1)

    -- refine the postcondition constraints
    mapM_ (uncurry fireTransitions) allTransitions

    -- save the current state and add changeable constraints
    transitionRng

    mapM_ (uncurry noTransitionTokens) [(t, p) | p <- places, t <- [0..(l-1)]]

    -- refine the must firers
    mustFireTransitions

    -- set new initial and final state
    setInitialState inputs places

    setFinalState (head rets) places

encoderRefine :: SplitInfo
              -> [TypeSkeleton]
              -> [TypeSkeleton]
              -> [EncodedFunction]
              -> Encoder ()
encoderRefine info inputs rets newSigs = do
    {- update the abstraction level -}
    modify $ over disabledTrans (++ removedTrans info)
    modify $ set returnTyps rets
    modify $ set (constraints . optionalConstraints) []
    modify $ set (constraints . finalConstraints) []

    {- operation on places -}
    l <- gets $ view pathLength
    t2tr <- gets $ view (variables . type2transition)
    let newPlaceIds = newPlaces info
    let newTransIds = newTrans info
    let currPlaces = Map.keys t2tr
    -- let newSigs = filter ((`elem` newTransIds) . funName) sigs
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
        transMap <- gets $ view (variables . transition2id)
        tsMap <- gets $ view (variables . time2variable)
        let trVar = lookupWithError "transition2id" tr transMap
        let tsVar = lookupWithError "time2variable" t tsMap
        eq <- mkEq tsVar trVar >>= mkNot
        modify $ over (constraints . persistConstraints) (eq :)
        incremental <- gets $ view (encSearchParams . incrementalSolving)
        when incremental $ assert eq

-- | add variables for each place
addPlaceVar ::  TypeSkeleton -> Int -> Encoder ()
addPlaceVar p t = do
    pvar <- gets $ view (variables . place2variable)
    vnbr <- gets $ view (variables . variableNb)
    placeVar <- mkZ3IntVar vnbr
    let p2v = Map.insert (p, t) placeVar pvar
    unless (Map.member (p, t) pvar) $ do
        modify $ set (variables . place2variable) p2v
        modify $ over (variables . variableNb) (+ 1)

-- | add transition mapping from (tr, lv) to integer id
-- an integer variable for each transition
addTransitionVar :: [Id] -> Encoder ()
addTransitionVar = mapM_ addTransitionVarFor
  where
    addTransitionVarFor tr = do
        trvar <- gets $ view (variables . transition2id)
        tid <- gets $ view (variables . transitionNb)
        trVar <- mkIntNum tid
        unless (Map.member tr trvar) $ do
            modify $ over (variables . transitionNb) (+ 1)
            modify $ over (variables . transition2id) (Map.insert tr trVar)
            modify $ over (variables . id2transition) (Map.insert tid tr)

addTimestampVar :: Int -> Encoder ()
addTimestampVar t = do
    tvar <- gets $ view (variables . time2variable)
    vnbr <- gets $ view (variables . variableNb)
    tsVar <- mkZ3IntVar vnbr
    unless (Map.member t tvar) $ do
        modify $ over (variables . time2variable) (Map.insert t tsVar)
        modify $ over (variables . variableNb) (+ 1)

-- | map each place and transition to a variable in z3
createVariables :: [TypeSkeleton] -> [Id] -> Encoder ()
createVariables places transitions = do
    l <- gets $ view pathLength
    -- add place variables
    mapM_ (uncurry addPlaceVar) [(a, i) | a <- places, i <- [0..l]]
    -- add transition mapping
    addTransitionVar transitions
    -- add timestamp variables
    mapM_ addTimestampVar [0..(l-1)]

createConstraints :: [TypeSkeleton] -> [EncodedFunction] -> Encoder ()
createConstraints places transitions = do
    l <- gets $ view pathLength
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- transitions]
    let allPlaces = [(t, p) | t <- [0..(l-1)], p <- places]

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

nonnegativeTokens :: [TypeSkeleton] -> Encoder ()
nonnegativeTokens places = do
    l <- gets $ view pathLength
    mapM_ (uncurry nonnegAt) [(p, t) | p <- places, t <- [0..l]]
  where
    nonnegAt p t = do
        placeMap <- gets $ view (variables . place2variable)
        let pVar = lookupWithError "placemap" (p, t) placeMap
        zero <- mkIntNum 0
        geZero <- mkGe pVar zero
        modify $ over (constraints . persistConstraints) (geZero :)
        incremental <- gets $ view (encSearchParams . incrementalSolving)
        when incremental $ assert geZero

-- | at each timestamp, only one transition can be fired, we restrict the
-- fired transition id range here
transitionRng :: Encoder ()
transitionRng = do
    l <- gets $ view pathLength
    mapM_ fireAt [0..(l-1)]
  where
    fireAt t = do
        tsMap <- gets $ view (variables . time2variable)
        transMax <- gets $ view (variables . transitionNb)
        let tsVar = lookupWithError "time2variable" t tsMap
        start <- mkIntNum 0
        geStart <- mkGe tsVar start
        end <- mkIntNum transMax
        ltEnd <- mkLt tsVar end
        modify $ over (constraints . optionalConstraints) ((ltEnd :) . (geStart :))

-- | if this place has no connected transition fired,
-- it has the same # of tokens
noTransitionTokens :: Int -> TypeSkeleton -> Encoder ()
noTransitionTokens t p = do
    trans <- gets $ view (variables . transition2id)
    t2tr <- gets $ view (variables . type2transition)
    let transSet = Set.toList $ Map.findWithDefault Set.empty p t2tr
    let transitions = map (\x -> lookupWithError "transition2id" x trans) transSet
    noFireLvs <- noFireAt transitions t
    noFire <- mkOr noFireLvs >>= mkNot
    placeMap <- gets $ view (variables . place2variable)
    let curr = lookupWithError "placemap" (p, t) placeMap
    let next = lookupWithError "placemap" (p, t + 1) placeMap
    tokenSame <- mkEq curr next
    noChange <- mkImplies noFire tokenSame
    modify $ over (constraints . optionalConstraints) (noChange :)
  where
    noFireAt transitions t = do
        tsMap <- gets $ view (variables . time2variable)
        let tsVar = lookupWithError "time2variable" t tsMap
        mapM (mkEq tsVar) transitions

fireTransitions :: Int -> EncodedFunction -> Encoder ()
fireTransitions t (EncodedFunction name params rets) = do
    transMap <- gets $ view (variables . transition2id)
    placeMap <- gets $ view (variables . place2variable)
    tsMap <- gets $ view (variables . time2variable)

    -- accumulate counting for parameters and return types
    let pcnt = map (\l -> (head l, length l)) (group (sort params))
    let pmap = Map.fromList pcnt
    let rmap = foldl' (\acc t -> Map.insertWith (+) t (-1) acc) pmap rets
    let rcnt = Map.toList rmap
    changes <- mapM (mkChange t) rcnt

    let tsVar = lookupWithError "time2variable" t tsMap
    let trVar = lookupWithError "transition2id" name transMap
    fire <- mkEq tsVar trVar
    enoughTokens <- mapM getSatisfiedPlace pcnt
    postCond <- mkAnd (enoughTokens ++ changes)
    tokenChange <- mkImplies fire postCond
    modify $ over (constraints . persistConstraints) (tokenChange :)
    incremental <- gets $ view (encSearchParams . incrementalSolving)
    when incremental $ assert tokenChange
  where
    mkChange t (p, diff) = do
        let d = -diff
        placeMap <- gets $ view (variables . place2variable)
        let before = lookupWithError "placemap" (p, t) placeMap
        let after = lookupWithError "placemap" (p, t + 1) placeMap
        diffw <- mkIntNum d
        mkAdd [before, diffw] >>= mkEq after

    getSatisfiedPlace (p, cnt) = do
        w <- mkIntNum cnt
        placeMap <- gets $ view (variables . place2variable)
        let pVar = lookupWithError "placemap" (p, t) placeMap
        mkGe pVar w

mustFireTransitions ::  Encoder ()
mustFireTransitions = do
    must <- gets $ view mustFirers
    mapM_ fireTransitionFor (Map.toList must)
  where
    nameInMust must name = foldr ((||) . (`Text.isInfixOf` name)) False must
    fireTransition tid = do
        l <- gets $ view pathLength
        tsMap <- gets $ view (variables . time2variable)
        let tsVars = map (\t -> lookupWithError "time2variable" t tsMap) [0..(l-1)]
        mapM (mkEq tid) tsVars

    fireTransitionFor (_, tids) = do
        transitions <- gets $ view (variables . transition2id)
        let mustTrans = Map.filterWithKey (\k _ -> nameInMust tids k) transitions
        fires <- mapM fireTransition mustTrans
        toFire <- mkOr (concat fires)
        modify $ over (constraints . optionalConstraints) (toFire :)
