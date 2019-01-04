{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module HooglePlus.Encoder where

import Control.Monad.State
import Z3.Monad hiding(Z3Env, newEnv)
import qualified Z3.Base as Z3
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Foldable
import Data.Maybe
import Data.Char

import Synquid.Type
import Synquid.Logic (ftrue)
import Synquid.Pretty
import Synquid.Util

data Z3Env = Z3Env {
    envSolver  :: Z3.Solver,
    envContext :: Z3.Context,
    envOptimize :: Z3.Optimize
}

-- we need a reader monad here to put the signatures and datatype indices
-- they would not be modified during the synthesis

data EncoderState = EncoderState {
    z3env      :: Z3Env             , -- solver environment
    signatures :: Map Id RSchema    , -- TODO: we may move to GHC structure here
    datatypes  :: Map (Id, Int) Int , -- index of dt in constructor list
    boundTvs   :: Set Id            , -- bounded type variables
    typeSort   :: Sort              , -- self defined sort with all types
    places     :: [AST]             , -- list of place mappings
    nameCounter :: Map Id Int         -- variable name counter
}

type Encoder = StateT EncoderState IO

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . z3env)
    getContext = gets (envContext . z3env)
    getOptimize = gets (envOptimize . z3env)

freshId :: Id -> Encoder Id
freshId prefix = do
    st <- get
    let indices = nameCounter st
    let idx = Map.findWithDefault 0 prefix indices
    put $ st { nameCounter = Map.insert prefix (idx+1) indices }
    return $ prefix ++ show idx

-- | Replace all bound type variables with fresh free variables
freshType :: RSchema -> Encoder RSchema
freshType sch = freshType' Map.empty sch
  where
    freshType' subst (ForallT (a, c) sch) = do
        a' <- freshId "t"
        sch' <- freshType' (Map.insert a (vart a' ftrue) subst) sch    
        return (ForallT (a', c) sch')
    freshType' subst (Monotype t) = return (Monotype (typeSubstitute subst $ t))

-- | fillDts: extracts all the datatypes from type signatures
fillDts :: Encoder ()
fillDts = do
    st <- get
    let dts = Set.unions $ map (allDatatypes . toMonotype)
                         $ Map.elems (signatures st)
    let tvs = Set.map (\t -> (t, 0)) (boundTvs st)
    let tyNames = map (\(t,c)-> (map toLower t, c)) $ Set.toList (dts `Set.union` tvs)
    let dtIdx = foldr (uncurry Map.insert) Map.empty $ zip tyNames [0,1..]
    put $ st { datatypes = dtIdx }
  where
    allDatatypes (FunctionT _ tArg tRet) = allDatatypes tArg `Set.union` allDatatypes tRet
    allDatatypes (ScalarT (DatatypeT id tArgs _) _) = (id, length tArgs) `Set.insert` foldr (Set.union . allDatatypes) Set.empty tArgs
    allDatatypes (ScalarT IntT _) = error "This type does not exist"
    allDatatypes (ScalarT BoolT _) = error "This type does not exist"
    allDatatypes (ScalarT (TypeVarT _ id) _) = Set.empty

-- | createType: create functions for each datatype as constructors in z3
-- strategy from Nadia, enumeration
createType :: Encoder ()
createType = do
    name <- mkStringSymbol "Type"
    dts <- sortOn snd . Map.toList . datatypes <$> get
    liftIO $ print dts
    tyConstructors <- mapM (uncurry addType) dts
    typ <- mkDatatype name tyConstructors
    st <- get
    put $ st { typeSort = typ }
  where
    mkParam name i = do
        sym <- mkStringSymbol (name ++ show i)
        return (sym, Nothing, 0)

    addType (name, kind) idx = do
        tname <- mkStringSymbol name
        -- assume all the types are unbounded variables
        params <- mapM (mkParam name) [1..kind]
        mkConstructor tname tname params
        
createPlaces :: Int -> Encoder ()
createPlaces l = do
    int <- mkIntSort
    typ <- typeSort <$> get
    parr <- mkArraySort typ int
    pls <- mapM (createPlaceAt parr) [0..l]
    st <- get
    put $ st { places = pls }
  where
    createPlaceAt parr i = do
        pi <- mkStringSymbol ("p" ++ show i)
        mkConst pi parr

addTvToMap :: Id -> Map Id AST -> Encoder (Map Id AST)
addTvToMap tv tm = do
    t <- mkStringSymbol tv
    typ <- typeSort <$> get
    tc <- mkConst t typ
    return $ Map.insert tv tc tm

mkTvMap :: [Id] -> Encoder (Map Id AST)
mkTvMap tvs = foldrM addTvToMap Map.empty tvs

mkArg :: Map Id AST -> SType -> Encoder AST
mkArg tvMap (ScalarT (TypeVarT _ id) r) = 
    case Map.lookup id tvMap of
        Just v -> return v
        Nothing -> mkArg tvMap (ScalarT (DatatypeT id [] []) r)
mkArg tvMap (ScalarT (DatatypeT id args _) _) = do
    -- when (id == "Bool") (liftIO $ putStrLn "this is a boolean constructors")
    -- create z3 arguments for this datatype function
    args' <- mapM (mkArg tvMap) args
    -- get the datatype function from constructors
    typ <- typeSort <$> get
    constructors <- getDatatypeSortConstructors typ
    dts <- datatypes <$> get
    let id' = map toLower id
    idx <- case Map.lookup (id', length args) dts of
             Just i  -> return i
             Nothing -> error $ "mkArg: cannot find such datatype " ++ show (id', length args)
    -- create type application
    mkApp (constructors !! idx) args'
mkArg tvMap t = error $ "mkArg: unsupported argument type " ++ show t

createFuncs :: Int -> Encoder ()
createFuncs l = do
    sigs <- signatures <$> get
    mapM_ createFunc [(f, t) | f <- Map.toList sigs, t <- [0..(l-1)]]
  where
    createFunc ((id, typ), i) = do
        liftIO $ print typ
        -- typ' <- freshType typ
        -- create a boolean variable per transition per time
        f <- mkStringSymbol (id ++ show i)
        bool <- mkBoolSort
        fc <- mkConst f bool

        -- create varaibles for each type variable in the current signature
        let tvs = boundVarsOf typ
        bounded <- boundTvs <$> get
        -- tm <- mkTvMap (tvs ++ Set.toList bounded)
        tm <- mkBoundTm (tvs ++ Set.toList bounded)

        uno <- mkIntNum 1
        -- let args = map (\a -> (head a, length a)) $ group $ allArgTypes (toMonotype typ')
        let args = allArgTypes (toMonotype typ)
        let ret = lastType (toMonotype typ)
        -- each input type has enough tokens to be consumed
        -- compute the impacted types and number of tokens first
        let decedMap = foldr decArgToken Map.empty args
        hasArgs <- mapM (uncurry (hasArgAt tm i)) (Map.toList decedMap)
        pls <- places <$> get
        -- each input type has to consume tokens to fire some transition
        let finalMap = Map.insertWith (+) (shape ret) 1 decedMap
        -- apply the final status
        -- consumedArgs <- foldrM (uncurry (decArgToken tm i)) (pls !! i) args
        -- produce the return type after the transition fired
        -- pAfter <- produceRetToken tm i ret consumedArgs
        pAfter <- foldrM (uncurry (applyArgToken tm i)) (pls !! i) (Map.toList finalMap)
        changeP <- mkEq (pls !! (i+1)) pAfter
        -- there exists some type variables to satisfy the requirements
        canFire <- mkAnd (changeP:hasArgs)
        -- tvSymbols <- mapM mkStringSymbol tvs
        liftIO $ print tvs
        tvSymbols <- mapM mkIntSymbol [(length tvs - 1),(length tvs -2)..0]
        styp <- typeSort <$> get
        let tvTypes = replicate (length tvs) styp
        fired <- if length tvSymbols > 0 
                   then mkExists [] tvSymbols tvTypes canFire >>= mkImplies fc
                   else mkImplies fc canFire
        -- optimizeAssert fired
        -- fired <- mkImplies fc canFire
        assert fired

    mkBoundTm tvs = foldrM addBoundTv Map.empty (zip tvs [0,1..])

    addBoundTv (tv, i) m = do
        typ <- typeSort <$> get
        x <- mkBound i typ
        return (Map.insert tv x m)

    hasArgAt tvMap i arg cnt = do
        pls <- places <$> get
        varg <- mkArg tvMap arg
        carg <- mkSelect (pls !! i) varg
        consumed <- mkIntNum (-cnt)
        mkGe carg consumed

    decArgToken arg mBefore = Map.insertWith (+) (shape arg) (-1) mBefore

    applyArgToken tvMap i arg cnt pBefore = do
        if cnt == 0
          then return pBefore
          else do
            pls <- places <$> get
            varg <- mkArg tvMap arg
            tBefore <- mkSelect (pls !! i) varg
            consumed <- mkIntNum cnt
            tAfter <- mkAdd [tBefore, consumed]
            mkStore pBefore varg tAfter
    {-
    produceRetToken tvMap i ret pBefore = do
        pls <- places <$> get
        vret <- mkArg tvMap ret
        tBefore <- mkSelect (pls !! i) vret
        produced <- mkIntNum 1
        tAfter <- mkAdd [tBefore, produced]
        mkStore pBefore vret tAfter
    -}
-- | set initial state for petri net solving
-- take a list of bounded type variables@tvs, argument types@args
-- return a new state
setInitial :: [Id] -> [RType] -> Encoder ()
setInitial tvs args = do
    typ <- typeSort <$> get
    -- liftIO $ print tvs
    -- tm <- mkTvMap tvs
    let tm = Map.empty
    -- prepare z3 variables for query arguments
    let cargs = map (\a -> (head a, length a)) (group args)
    
    int <- mkIntSort
    parr <- mkArraySort typ int
    zero <- mkIntNum 0
    emptyArr <- mkConstArray typ zero
    assignedArr <- foldrM (assignArg tm) emptyArr cargs
    pls <- places <$> get
    initial <- mkEq (pls !! 0) assignedArr
    -- optimizeAssert initial
    assert initial
  where
    assignArg tm (tArg, cnt) vArr = do
        vArg <- mkArg tm (shape tArg)
        vCnt <- mkIntNum cnt
        mkStore vArr vArg vCnt

-- | set final state for petri net solving
-- take a list of bounded type variables@tvs and return type@tRet
setFinal :: [Id] -> RType -> Int -> Encoder ()
setFinal tvs tRet i = do
    typ <- typeSort <$> get
    -- tm <- mkTvMap tvs
    let tm = Map.empty

    -- constraint: only one token in the return type and all the others have none
    int <- mkIntSort
    parr <- mkArraySort typ int
    zero <- mkIntNum 0
    emptyArr <- mkConstArray typ zero
    vRet <- mkArg tm (shape tRet)
    uno <- mkIntNum 1
    assignedArr <- mkStore emptyArr vRet uno

    pls <- places <$> get
    final <- mkEq (pls !! i) assignedArr
    -- optimizeAssert final
    assert final

encode :: [Id] -> [RType] -> RType -> Int -> Encoder ()
encode tvs args tRet i = do
    -- optimizePush
    push
    liftIO $ print $ "trying length of " ++ show i
    -- create list of place arrays
    createPlaces i
    -- create constraints
    createFuncs i
    -- set initial state
    setInitial tvs args
    -- set final state
    setFinal tvs tRet i
    -- at least one transition is fired at each time
    mapM_ atExactlyOne [0..(i-1)]

    -- assts <- solverAssertions
    -- asstStr <- mapM astToString assts
    -- liftIO $ mapM_ putStrLn asstStr
    solverStr <- solverToString
    liftIO $ putStrLn solverStr
    -- check by the solver
    -- res <- optimizeCheck
    res <- solverCheck
    case res of 
        Sat -> do
            -- model <- optimizeGetModel
            model <- solverGetModel
            modelStr <- modelToString model
            liftIO $ putStrLn modelStr
        _ -> do
            liftIO $ putStrLn "Here UNSAT"
            when (i < 3) $ do
                -- unsatCore <- solverGetUnsatCore
                -- asstStr <- mapM astToString unsatCore
                -- liftIO $ mapM_ putStrLn asstStr
                {- optimizePop 0 -}
                -- pop 1
                solverReset
                encode tvs args tRet (i+1)
  where
    -- fire only one transition at each timestamp
    atExactlyOne t = do
        bool <- mkBoolSort
        sigs <- signatures <$> get
        fs <- mapM (\name -> mkStringSymbol (name ++ show t)) (Map.keys sigs)
        fc <- mapM (\sym -> mkConst sym bool) fs
        -- at least one transition is fired at the current timestamp
        -- mkOr fc >>= optimizeAssert
        mkOr fc >>= assert
        -- at most one transition is fired at the current timestamp
        -- mapM_ (fireAt fc) [0..(length fc - 1)]

    fireAt fc i = do
        let selected = fc !! i
        let other = deleteAt i fc
        mkOr other >>= mkNot >>= mkImplies selected >>= assert -- optimizeAssert

newEnv :: Maybe Logic -> Opts -> IO Z3Env
newEnv mbLogic opts =
  Z3.withConfig $ \cfg -> do
    setOpts cfg opts
    ctx <- Z3.mkContext cfg
    opt <- Z3.mkOptimize ctx
    solver <- maybe (Z3.mkSolver ctx) (Z3.mkSolverForLogic ctx) mbLogic
    return $ Z3Env solver ctx opt

initialZ3Env = newEnv Nothing stdOpts

runTest :: [Id] -> [RType] -> RType -> Encoder ()
runTest tvs args ret = do
    fillDts
    createType
    encode tvs args ret 1
