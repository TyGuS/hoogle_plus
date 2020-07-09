{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Encoder.Z3SATEnc () where

import Data.Maybe
import Data.List
import Data.List.Extra
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
import Control.Lens

import Encoder.Z3SATTypes
import Encoder.ConstraintEncoder (FunctionCode(..))
import qualified Encoder.ConstraintEncoder as CE
import Encoder.Utils
import Types.Common
import Types.Experiments
import Types.Type
import PetriNet.AbstractType
import PetriNet.Utils
import Synquid.Utils
import Synquid.Pretty

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . _z3env)
    getContext = gets (envContext . _z3env)

-- | create a new encoder in z3
createEncoder :: [AbstractSkeleton] -> AbstractSkeleton -> [FunctionCode] -> Encoder ()
createEncoder inputs ret sigs = do
    ty2tr <- gets $ view (variables . type2transition)
    let places = HashMap.keys ty2tr
    let transIds = Set.toList $ Set.unions $ HashMap.elems ty2tr
    -- create all the type variables for encoding
    createVariables places transIds
    -- add all the constraints for the solver
    createConstraints places sigs
    -- set initial and final state for solver
    setInitialState inputs places
    setFinalState ret places

assignToken cons p v = do
    placeMap <- gets $ view (variables . place2variable)
    let tVar = findVariable "place2variable" (p, 0, v) placeMap
    modify $ over (constraints . cons) (tVar :)

-- | set the initial state for the solver, where we have tokens only in void or inputs
-- the tokens in the other places should be zero
setInitialState :: [AbstractSkeleton] -> [AbstractSkeleton] -> Encoder ()
setInitialState inputs places = do
    let nonInputs = filter (`notElem` inputs) places
    let inputCounts = map (\l -> (head l, length l)) (group (sort inputs))
    let nonInputCounts = map (,0) nonInputs
    mapM_ (uncurry $ assignToken optionalConstraints) (inputCounts ++ nonInputCounts)

-- | set the final solver state, we allow only one token in the return type
-- and maybe several tokens in the "void" place
setFinalState :: AbstractSkeleton -> [AbstractSkeleton] -> Encoder ()
setFinalState ret places = do
    let nonOutputs = filter (ret /=) places
    let nonOutputCounts = map (, 0) nonOutputs
    let outputCount = (ret, 1)
    mapM_ (uncurry $ assignToken finalConstraints) (outputCount : nonOutputCounts)

getParam :: Encoder (Int, Z3.Sort)
getParam = do
    cnt <- gets $ view (increments . counter)
    boolS <- mkBoolSort
    return (cnt, boolS)

addAllConstraints :: Encoder ()
addAllConstraints = do
    Constraints p o f b <- gets $ view constraints
    mapM_ assert p
    mapM_ assert o
    mapM_ assert f
    mapM_ assert b

nonincrementalSolve :: Encoder Z3.Result
nonincrementalSolve = do
    prev <- gets $ view (increments . prevChecked)
    when prev $ do
        toBlock <- gets $ view (increments . block)
        modify $ over (constraints . blockConstraints) (toBlock :)
        modify $ set (increments . prevChecked) False

    addAllConstraints
    check

solveAndGetModel :: Encoder [Id]
solveAndGetModel = do
    l <- gets $ view (increments . loc)
    res <- nonincrementalSolve

    case res of
        Sat -> do
            model <- solverGetModel
            transMap <- gets $ view (variables . trans2variable)
            -- evaluate what transitions are fired
            selected <- filterM (uncurry $ checkTrans model) (HashMap.toList transMap)
            let selectedTrans = map fst selected
            let transVars = HashMap.elems transMap
            blockAss <- mkAnd transVars >>= mkNot
            modify $ set (increments . block) blockAss
            solverReset
            return $ map fst $ sortOn snd selectedTrans
        Unsat -> do
            rets <- gets $ view (refinements . returnTyps)
            solverReset
            if length rets == 1
              then return []
              else do
                -- liftIO $ print "unsat for change goal"
                -- try a more general return type
                t2tr <- gets $ view (variables . type2transition)
                modify $ set (constraints . finalConstraints) []
                modify $ set (refinements . returnTyps) (tail rets)
                modify $ set (increments . prevChecked) False
                setFinalState (rets !! 1) (HashMap.keys t2tr)
                solveAndGetModel
        Undef -> return []
  where
    checkTrans model (f, t) v = do
        bMay <- evalBool model v
        case bMay of
            Just b -> return b
            Nothing -> error $ "cannot eval the variable " ++ show (f, t)

encoderInit :: Z3SATState
            -> Int
            -> [AbstractSkeleton]
            -> [AbstractSkeleton]
            -> [FunctionCode]
            -> IO Z3SATState
encoderInit encoderState len inputs rets sigs = do
    z3Env <- initialZ3Env
    false <- Z3.mkFalse (envContext z3Env)
    execStateT (do
        modify $ set z3env z3Env
        modify $ set (increments . loc) len
        modify $ set (increments . encodedSigs) sigs
        modify $ set (refinements . returnTyps) rets
        modify $ set (refinements . inputTyps) inputs
        createEncoder inputs (head rets) sigs) encoderState

encoderSolve :: Z3SATState -> IO ([Id], Z3SATState)
encoderSolve = runStateT solveAndGetModel

-- optimize the optional constraints here:
-- we only need to change the must firers and noTransitionTokens and final states
encoderInc :: [FunctionCode] -> [AbstractSkeleton] -> [AbstractSkeleton] -> Encoder ()
encoderInc sigs inputs rets = do
    modify $ over (increments . loc) (+ 1)
    modify $ set (refinements . returnTyps) rets
    persists <- gets $ view (constraints . persistConstraints)
    modify $ set constraints ( emptyConstraints { _persistConstraints = persists })

    ty2tr <- gets $ view (variables . type2transition)
    let places = HashMap.keys ty2tr
    let transitions = Set.toList $ Set.unions $ HashMap.elems ty2tr
    l <- gets $ view (increments . loc)

    -- add new place, transition and timestamp variables
    let allPlaces = [(p, t) | p <- places, t <- [0..(l-1)]]
    mapM_ (uncurry addPlaceVar) [(p, l) | p <- places]

    let allTransitions = [(tr, l - 1) | tr <- sigs ]
    mapM_ (uncurry addTransitionVar . over _1 funName) allTransitions

    -- disable transitions at the new timestamp
    toRemove <- gets $ view (refinements . disabledTrans)
    disableTransitions toRemove (l - 1)

    -- refine the postcondition constraints
    mapM_ (uncurry fireTransitions) allTransitions

    mapM_ (uncurry noTransitionTokens) allPlaces

    noOverlapTransitions

    -- refine the must firers
    mustFireTransitions

    -- set new initial and final state
    setInitialState inputs places

    setFinalState (head rets) places

encoderRefine :: SplitInfo
              -> [AbstractSkeleton]
              -> [AbstractSkeleton]
              -> [FunctionCode]
              -> Encoder ()
encoderRefine info inputs rets newSigs = do
    {- update the abstraction level -}
    modify $ over (refinements . disabledTrans) (++ removedTrans info)
    modify $ over (increments . encodedSigs) (++ newSigs)
    modify $ set (refinements . returnTyps) rets
    modify $ set (refinements . inputTyps) inputs
    modify $ set (constraints . optionalConstraints) []
    modify $ set (constraints . finalConstraints) []

    {- operation on places -}
    l <- gets $ view (increments . loc)
    t2tr <- gets $ view (variables . type2transition)
    let newPlaceIds = newPlaces info
    let newTransIds = newTrans info
    let currPlaces = HashMap.keys t2tr
    let allTrans = [(tr, t) | t <- [0..(l - 1)], tr <- newSigs]

    -- add new place, transition and timestamp variables
    mapM_ (uncurry addPlaceVar) [(p, t) | p <- newPlaceIds, t <- [0..l]]
    mapM_ (uncurry addTransitionVar) [(tr, t) | tr <- newTransIds, t <- [0..(l - 1)]]

    -- refine the postcondition constraints
    mapM_ (uncurry fireTransitions) allTrans

    -- disable splitted transitions
    mapM_ (disableTransitions (removedTrans info)) [0..(l-1)]

    mapM_ (uncurry noTransitionTokens) [(p, t) | p <- currPlaces, t <- [0..(l-1)]]

    noOverlapTransitions

    -- refine the must firers
    mustFireTransitions

    -- set new initial and final state
    setInitialState inputs currPlaces

    setFinalState (head rets) currPlaces

disableTransitions :: [Id] -> Int -> Encoder ()
disableTransitions trs t = mapM_ disableTrAt trs
  where
    disableTrAt tr = do
        transMap <- gets $ view (variables . trans2variable)
        let trVar = findVariable "trans2variable" (tr, t) transMap
        eq <- mkNot trVar
        modify $ over (constraints . persistConstraints) (eq :)

-- max number of tokens for each type
-- either the number of tokens in the input types
-- or the number of tokens to be consumed
maxToken :: AbstractSkeleton -> Encoder Int
maxToken ty = do
    t2tr <- gets $ view (variables . type2transition)
    signatures <- gets $ view (increments . encodedSigs)
    inputs <- gets $ view (refinements . inputTyps)
    let inputCounts = map (\l -> (head l, length l)) (group $ sort inputs)
    let cnt = fromMaybe 0 (lookup ty inputCounts)
    let connects = HashMap.lookupDefault Set.empty ty t2tr
    let consume (FunctionCode f args _) = if f `Set.member` connects then length (filter (== ty) args) else 0
    return $ (+ 1) . maximum $ cnt : map consume signatures

-- | add variables for each place
addPlaceVar :: AbstractSkeleton -> Int -> Encoder ()
addPlaceVar p t = do
    mt <- maxToken p
    mapM_ (uncurry addPlaceVarFor) [(t, m) | m <- [0..mt]]
    where
        addPlaceVarFor t m = do
            pvar <- gets $ view (variables . place2variable)
            vnbr <- gets $ view (variables . variableNb)
            placeVar <- mkZ3Var vnbr
            let p2v = HashMap.insert (p, t, m) placeVar pvar
            unless (HashMap.member (p, t, m) pvar) $ do
                modify $ set (variables . place2variable) p2v
                modify $ over (variables . variableNb) (+ 1)

-- | add transition mapping from (tr, lv) to integer id
-- an integer variable for each transition
addTransitionVar :: Id -> Int -> Encoder ()
addTransitionVar tr t = do
    transMap <- gets $ view (variables . trans2variable)
    tid <- gets $ view (variables . transitionNb)
    trVar <- mkZ3Var tid
    unless (HashMap.member (tr, t) transMap) $ do
        modify $ over (variables . transitionNb) (+ 1)
        modify $ over (variables . trans2variable) (HashMap.insert (tr, t) trVar)

mkZ3Var :: Int -> Encoder AST
mkZ3Var var = do
    varSymbol <- mkIntSymbol var
    boolS <- mkBoolSort
    mkConst varSymbol boolS

-- | map each place and transition to a variable in z3
createVariables :: [AbstractSkeleton] -> [Id] -> Encoder ()
createVariables places transitions = do
    l <- gets $ view (increments . loc)
    -- add place variables
    mapM_ (uncurry addPlaceVar) [(p, t) | p <- places, t <- [0..l]]
    -- add transition mapping
    mapM_ (uncurry addTransitionVar) [(tr, t) | tr <- transitions, t <- [0..(l-1)]]

createConstraints :: [AbstractSkeleton] -> [FunctionCode] -> Encoder ()
createConstraints places transitions = do
    -- prepare constraint parameters
    -- liftIO $ print places
    l <- gets $ view (increments . loc)
    let allTrans = [(tr, t) | t <- [0..(l-1)], tr <- transitions]
    let allPlaces = [(p, t) | t <- [0..(l-1)], p <- places]

    mapM_ (uncurry fireTransitions) allTrans

    mapM_ (uncurry noTransitionTokens) allPlaces

    noOverlapTransitions

    mustFireTransitions

-- | if this place has no connected transition fired,
-- it has the same # of tokens
noTransitionTokens :: AbstractSkeleton -> Int -> Encoder ()
noTransitionTokens p t = do
    t2tr <- gets $ view (variables . type2transition)
    transMap <- gets $ view (variables . trans2variable)
    mt <- maxToken p
    let transList = Set.toList $ HashMap.lookupDefault Set.empty p t2tr
    let noFireLvs = map (\tr -> findVariable "trans2variable" (tr, t) transMap) transList
    noFire <- mkOr noFireLvs >>= mkNot
    noChanges <- mapM (noFireAt noFire) [0..mt]
    modify $ over (constraints . optionalConstraints) (++ noChanges)
  where
    noFireAt noFire m = do
        placeMap <- gets $ view (variables . place2variable)
        let curr = findVariable "place2variable" (p, t, m) placeMap
        let next = findVariable "place2variable" (p, t + 1, m) placeMap
        tokenSame <- mkEq curr next
        mkImplies noFire tokenSame

fireTransitions :: FunctionCode -> Int -> Encoder ()
fireTransitions (FunctionCode name params rets) t = do
    transMap <- gets $ view (variables . trans2variable)
    placeMap <- gets $ view (variables . place2variable)

    -- accumulate counting for parameters and return types
    let pcnt = map (\l -> (head l, length l)) (group (sort params))
    let pmap = HashMap.fromList pcnt
    let rmap = foldl' (\acc t -> HashMap.insertWith (+) t (-1) acc) pmap rets
    let rcnt = HashMap.toList rmap
    let trVar = findVariable "transition2id" (name, t) transMap

    allChanges <- mapM mkChange rcnt
    postCond <- mkAnd allChanges
    tokenChange <- mkImplies trVar postCond
    modify $ over (constraints . persistConstraints) (tokenChange :)
  where
    mkChange (p, diff) = do
        placeMap <- gets $ view (variables . place2variable)
        mt <- maxToken p

        -- precondition: have enough tokens to be consumed for transition firing
        -- postcondition: tokens change to the correct number
        let start = if diff > 0 then diff else 0
        let end = if diff < 0 then mt + diff else mt
        changes <- mapM (\m -> do
            let before = findVariable "placemap" (p, t, m) placeMap
            let after = findVariable "placemap" (p, t + 1, m - diff) placeMap
            mkAnd [before, after]
            ) [start..end]
        mkOr changes

mustFireTransitions :: Encoder ()
mustFireTransitions = do
    must <- gets $ view (refinements . mustFirers)
    mapM_ fireTransitionFor (HashMap.toList must)
  where
    nameInMust must name = foldr ((||) . flip isInfixOf name) False must
    fireTransition tid = do
        l <- gets $ view (increments . loc)
        transMap <- gets $ view (variables . trans2variable)
        let trVars = map (\t -> findVariable "trans2variable" (tid, t) transMap) [0..(l-1)]
        mkOr trVars

    fireTransitionFor (_, tids) = do
        fires <- mapM fireTransition tids
        toFire <- mkOr fires
        modify $ over (constraints . optionalConstraints) (toFire :)

noOverlapTransitions :: Encoder ()
noOverlapTransitions = do
    -- transitions with the same arg/ret type cannot be fired together
    t2tr <- gets $ view (variables . type2transition)
    l <- gets $ view (increments . loc)
    mapM_ (uncurry noOverlapFor) [(Set.toList tids, t) | (_, tids) <- HashMap.toList t2tr
                                                       , t <- [0..(l - 1)]]
    where
        noOverlapFor tids t = do
            transMap <- gets $ view (variables . trans2variable)
            let trVars = map (\tr -> findVariable "trans2variable" (tr, t) transMap) tids
            mapM_ (onlyOne trVars) trVars

        onlyOne all selected = do
            let others = delete selected all
            noOthers <- mkOr others >>= mkNot
            oneFire <- mkImplies selected noOthers
            modify $ over (constraints . optionalConstraints) (oneFire :)

instance CE.ConstraintEncoder Z3SATState where
    encoderInit = encoderInit
    encoderInc sigs inputs rets = execStateT (encoderInc sigs inputs rets)
    encoderRefine info inputs rets newSigs = execStateT (encoderRefine info inputs rets newSigs)
    encoderSolve = encoderSolve

    emptyEncoder = emptyZ3SATState
    getTy2tr enc = enc ^. variables . type2transition
    setTy2tr m = variables . type2transition .~ m
    modifyTy2tr f = variables . type2transition %~ f
    setPrevChecked c = increments . prevChecked .~ c
    modifyMusters f = refinements . mustFirers %~ f
    setParams p = encSearchParams .~ p
