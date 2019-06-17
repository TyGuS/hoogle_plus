{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import PetriNet.Util
import Synquid.Util
import Synquid.Pretty

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . z3env)
    getContext = gets (envContext . z3env)
    getOptimize = gets (envOptimize . z3env)

-- | create a new encoder in z3
createEncoder :: [Id] -> Id -> Encoder ()
createEncoder inputs ret = do
    places <- gets ((:) "void" . HashMap.keys . ty2tr)
    transIds <- gets (nubOrd . concat . HashMap.elems . ty2tr)
    -- create all the type variables for encoding
    createVariables places transIds
    -- add all the constraints for the solver
    createConstraints places

    push
    transitionRng
    mustFireTransitions
    -- set initial and final state for solver
    setInitialState inputs places
    push
    setFinalState ret places

-- | set the initial state for the solver, where we have tokens only in void or inputs
-- the tokens in the other places should be zero
setInitialState :: [Id] -> [Id] -> Encoder ()
setInitialState inputs places = do
    let nonInputs = filter (\k -> notElem k inputs) places
    let inputCounts = map (\t -> (head t, length t)) (group (sort inputs))
    let nonInputCounts = map (\t -> (t, if t == "void" then 1 else 0)) nonInputs
    let typeCounts = inputCounts ++  nonInputCounts
    -- assign tokens to each types
    mapM_ (uncurry assignToken) typeCounts
  where
    assignToken p v = do
        placeMap <- place2variable <$> get
        tVar <- mkZ3IntVar $ findVariable (p, 0) placeMap
        eq <- mkIntNum v >>= mkEq tVar
        assert eq

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
        placeMap <- gets place2variable
        l <- gets loc
        retVar <- mkZ3IntVar $ findVariable (ret, l) placeMap
        assrt <- mkIntNum 1 >>= mkEq retVar
        assert assrt

    excludeOther p = do
        l <- gets loc
        placeMap <- gets place2variable
        when (p /= "void") $ do
            tVar <- mkZ3IntVar $ findVariable (p, l) placeMap
            eq <- mkIntNum 0 >>= mkEq tVar
            assert eq

solveAndGetModel :: Encoder [(Id, Int)]
solveAndGetModel = do
    prev <- gets prevChecked
    l <- gets loc
    when prev $ do
        toBlock <- gets block
        assert toBlock
    {-
    solverStr <- solverToString
    cnt <- gets counter
    liftIO $ print cnt
    hout <- liftIO $ openFile ("firstJustQQ" ++ show cnt) WriteMode
    liftIO $ hPutStrLn hout solverStr
    liftIO $ hClose hout
    modify $ \s -> s { counter = cnt + 1 }
    -}

    res <- check
    
    when (l == 3) $ do
      solverStr <- solverToString
      liftIO $ putStrLn solverStr
      transMap <- transition2id <$> get
      liftIO $ print transMap
      placeMap <- place2variable <$> get
      liftIO $ print placeMap
      transMap' <- id2transition <$> get
      liftIO $ print transMap'

    case res of
        Sat -> do
            liftIO $ print "sat"
            model <- solverGetModel
            places <- gets (HashMap.keys . ty2tr)
            selected <- mapM (checkLit model) [0..(l-1)]
            placed <- mapM (uncurry $ checkPlace model) [(p, t) | p <- places
                                                                , t <- [0..l]]
            blockTrs <- mapM (uncurry blockTr) (zip [0..(l-1)] selected)
            blockAss <- mkAnd (placed ++ blockTrs) >>= mkNot
            modify $ \st -> st { block = blockAss }
            selectedNames <- getTrNames selected
            return (zip selectedNames [0,1..])
        Unsat -> do
            rets <- gets returnTyps
            if length rets == 1
              then do
                liftIO $ print "unsat for inc path"
                return []
              else do
                pop 1
                liftIO $ print "unsat for change goal"
                -- try a more general return type
                t2tr <- gets ty2tr
                modify $ \st -> st { returnTyps = tail rets 
                                   , prevChecked = False }
                push
                setFinalState (rets !! 1) (HashMap.keys t2tr)
                solveAndGetModel
        Undef -> do
            return []
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

    checkIntLit model (k, v) = do
        pVar <- mkZ3IntVar v
        iMay <- eval model pVar
        case iMay of
            Just i -> mkEq pVar i
            Nothing -> error $ "cannot eval the variable" ++ show k

encoderInit :: Int -> HashMap Id [Id] -> [Id] -> [Id] -> HashMap Id FunctionCode -> HashMap Id [Id] -> IO EncodeState
encoderInit loc hoArgs inputs rets sigs t2tr = do
    z3Env <- initialZ3Env
    false <- Z3.mkFalse (envContext z3Env)
    let initialState = EncodeState z3Env 0 false loc 0 1 HashMap.empty HashMap.empty HashMap.empty HashMap.empty HashMap.empty hoArgs t2tr sigs False [] rets
    execStateT (createEncoder inputs (head rets)) initialState

encoderSolve :: EncodeState -> IO ([(Id, Int)], EncodeState)
encoderSolve st = runStateT solveAndGetModel st

encoderInc :: HashMap Id FunctionCode -> [Id] -> [Id] -> Encoder ()
encoderInc sigs inputs rets = do
    pop 2

    modify $ \st -> st { loc = loc st + 1
                       , returnTyps = rets
                       , functionCodes = sigs }
    t2tr <- gets ty2tr
    l <- gets loc
    pre <- mkFuncVar "pre"
    post <- mkFuncVar "post"
    let places = "void" : HashMap.keys t2tr
    let transitions = nubOrd $ concat $ HashMap.elems t2tr
    let newPlaceTime = [(a, l - 1) | a <- places]

    -- add new place, transition and timestamp variables
    mapM_ (uncurry addPlaceVarAt) [(a, l) | a <- places]
    addTimestampVar (l - 1)

    -- all places have non-negative number of tokens
    nonnegativeTokens places

    -- disable transitions at the new timestamp
    toRemove <- gets disabledTrans
    disableTransitions toRemove (l-1)

    -- have enough tokens to be consumed before firing
    mapM_ (uncurry $ enoughTokens pre) newPlaceTime
    
    -- change token numbers after transition fired
    mapM_ (uncurry $ fireTransitions post) newPlaceTime

    push
    -- refine the must firers
    mustFireTransitions
    -- save the current state and add changeable constraints
    transitionRng
    -- set new initial and final state
    setInitialState inputs places
    push
    setFinalState (head rets) places

encoderRefine :: SplitInfo -> HashMap Id FunctionCode -> HashMap Id [Id] -> [Id] -> [Id] -> HashMap Id [Id] -> Encoder ()
encoderRefine info sigs t2tr inputs rets musters = do
    pop 2
    {- update the abstraction level -}
    modify $ \st -> st { ty2tr = t2tr 
                       , mustFirers = musters
                       , disabledTrans = disabledTrans st ++ removedTrans info
                       , returnTyps = rets
                       , functionCodes = sigs
                       }

    {- operation on places -}
    l <- gets loc
    pre <- mkFuncVar "pre"
    post <- mkFuncVar "post"
    let newPlaceIds = map show (newPlaces info)
    let newTransIds = newTrans info
    let newPlaceTime = [(a, i) | a <- newPlaceIds, i <- [0..(l-1)]]
    let places = HashMap.keys t2tr
    let transitions = concat $ HashMap.elems t2tr
    let newPlAllTrans = [(a, t) | a <- newPlaceIds, t <- transitions]
    let allPlNewTrans = [(a, t) | a <- places, t <- newTransIds]

    -- add new place, transition variables
    mapM_ addPlaceId newPlaceIds
    mapM_ (uncurry addPlaceVarAt) [(a, i) | a <- newPlaceIds, i <- [0..l]]
    addTransitionVar newTransIds

    -- add new definitions for pre and post function
    mapM_ (uncurry $ precondition pre) newPlAllTrans
    mapM_ (uncurry $ precondition pre) allPlNewTrans
    mapM_ (uncurry $ postcondition post) newPlAllTrans
    mapM_ (uncurry $ postcondition post) allPlNewTrans

    -- all places have non-negative number of tokens
    nonnegativeTokens newPlaceIds
    -- disable splitted transitions
    mapM_ (disableTransitions $ removedTrans info) [0..(l-1)]
    -- enough tokens to be consumed
    mapM_ (uncurry $ enoughTokens pre) newPlaceTime
    -- token number changed after transition fired
    mapM_ (uncurry $ fireTransitions post) newPlaceTime

    push
    -- update the maximum transition index
    transitionRng
    -- refine the must firers
    mustFireTransitions
    -- initial markings
    setInitialState inputs places

    push
    -- final markings
    setFinalState (head rets) places

disableTransitions :: [Id] -> Int -> Encoder ()
disableTransitions trs t = do
    mapM_ disableTrAt trs
  where
    disableTrAt tr = do
        transMap <- gets transition2id
        tsMap <- gets time2variable
        trVar <- mkIntNum (findVariable tr transMap)
        tsVar <- mkZ3IntVar (findVariable t tsMap)
        eq <- mkEq tsVar trVar >>= mkNot
        assert eq

-- | add variables for each place:
addPlaceVarAt :: Id -> Int -> Encoder ()
addPlaceVarAt p t = do
    st <- get
    let placeVar = Variable (variableNb st) (p ++ "_" ++ show t) t 0 VarPlace
    let p2v = HashMap.insert (p, t) placeVar $ place2variable st
    put $ st { place2variable = p2v
             , variableNb = variableNb st + 1 }

addPlaceId :: Id -> Encoder ()
addPlaceId p = do
    st <- get
    let p2id = HashMap.insert p (variableNb st) (place2id st)
    put $ st { place2id = p2id
             , variableNb = variableNb st + 1 }

-- | add transition mapping from (tr, lv) to integer id
-- 1) an integer variable for each transition
-- 2) an fresh integer variable for each higher-order argument
addTransitionVar :: [Id] -> Encoder ()
addTransitionVar trs = mapM_ addTransitionVarFor trs
  where
    addTransitionVarFor tr = do
        st <- get
        let tid = transitionNb st
        when (not (HashMap.member tr (transition2id st)))
             (put $ st { transitionNb = 1 + transitionNb st
                       , transition2id = HashMap.insert tr tid (transition2id st)
                       , id2transition = HashMap.insert tid tr (id2transition st)
                       })

addTimestampVar :: Int -> Encoder ()
addTimestampVar t = do
    st <- get
    let tsVar = Variable (variableNb st) ("ts_" ++ show t) t 0 VarTimestamp
    when (not (HashMap.member t (time2variable st)))
         (put $ st { time2variable = HashMap.insert t tsVar (time2variable st)
                   , variableNb = variableNb st + 1
                   })

-- | map each place and transition to a variable in z3
createVariables :: [Id] -> [Id] -> Encoder ()
createVariables places transitions = do
    l <- gets loc
    pre <- mkFuncVar "pre"
    post <- mkFuncVar "post"
    -- add place variables
    mapM_ addPlaceId places
    mapM_ (uncurry addPlaceVarAt) [(a, i) | a <- places, i <- [0..l]]
    -- add transition mapping
    addTransitionVar transitions
    -- add timestamp variables
    mapM_ addTimestampVar [0..(l-1)]
    -- define pre and post functions
    mapM_ (uncurry $ precondition pre) [(a, t) | a <- places, t <- transitions]
    mapM_ (uncurry $ postcondition post) [(a, t) | a <- places, t <- transitions]

mkFuncVar :: Id -> Encoder Z3.FuncDecl
mkFuncVar id = do
    intS <- mkIntSort
    funSym <- mkStringSymbol id
    funDecl <- mkFuncDecl funSym [intS, intS] intS
    return funDecl

createConstraints :: [Id] -> Encoder ()
createConstraints places = do
    l <- gets loc
    preFunc <- mkFuncVar "pre"
    postFunc <- mkFuncVar "post"
    let allPlaceTime = [(a, i) | a <- places, i <- [0..(l-1)]]
    nonnegativeTokens places
    mapM_ (uncurry $ enoughTokens preFunc) allPlaceTime
    mapM_ (uncurry $ fireTransitions postFunc) allPlaceTime

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
        assert geStart
        assert ltEnd

precondition :: Z3.FuncDecl -> Id -> Id -> Encoder ()
precondition preFunc a t = do
    fcMap <- gets functionCodes
    let fc = findVariable t fcMap
    let params = funParams fc
    defineFunc preFunc a t $ if a `elem` params then count a params else 0

postcondition :: Z3.FuncDecl -> Id -> Id -> Encoder ()
postcondition postFunc a t = do
    fcMap <- gets functionCodes
    let fc = findVariable t fcMap
    let tyList = funParams fc ++ funReturn fc
    let diff = count a (funReturn fc) - count a (funParams fc)
    defineFunc postFunc a t $ if a `elem` tyList then diff else 0
    
defineFunc :: Z3.FuncDecl -> Id -> Id -> Int -> Encoder ()
defineFunc func a t w = do
    placeMap <- gets place2id
    transMap <- gets transition2id
    aVar <- mkIntNum $ findVariable a placeMap
    tVar <- mkIntNum $ findVariable t transMap
    wVar <- mkIntNum w
    applyFun <- mkApp func [aVar, tVar]
    eq <- mkEq applyFun wVar
    assert eq

mustFireTransitions :: Encoder ()
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
        trVars <- mapM (mkEq trId) tsVars
        return trVars
    fireTransitionFor (id, tids) = do
        transitions <- gets transition2id
        let mustTrans = HashMap.filterWithKey (\k _ -> nameInMust tids k) transitions
        fires <- mapM fireTransition mustTrans
        toFire <- mkOr (concat fires)
        assert toFire

prepVars :: Id -> Int -> Encoder (Z3.AST, Z3.AST, Z3.AST, Z3.AST)
prepVars a i = do
    placeMap <- gets place2id
    tsMap <- gets time2variable
    ptMap <- gets place2variable
    aVar <- mkIntNum $ findVariable a placeMap
    iVar <- mkZ3IntVar $ findVariable i tsMap
    ptVar <- mkZ3IntVar $ findVariable (a, i) ptMap
    ptnVar <- mkZ3IntVar $ findVariable (a, i + 1) ptMap
    return (aVar, iVar, ptVar, ptnVar)

enoughTokens :: Z3.FuncDecl -> Id -> Int -> Encoder ()
enoughTokens pre a i = do
    (aVar, iVar, ptVar, _) <- prepVars a i
    applyFun <- mkApp pre [aVar, iVar]
    enough <- mkLe applyFun ptVar
    assert enough

fireTransitions :: Z3.FuncDecl -> Id -> Int -> Encoder ()
fireTransitions post a i = do
    (aVar, iVar, ptVar, ptnVar) <- prepVars a i
    applyFun <- mkApp post [aVar, iVar]
    consume <- mkAdd [applyFun, ptVar]
    eq <- mkEq consume ptnVar
    assert eq
