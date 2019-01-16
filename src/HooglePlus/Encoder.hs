{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module HooglePlus.Encoder where

import Control.Monad.State
import Control.Monad.Extra
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

data EncoderType = Normal | Arity

data Z3Env = Z3Env {
    envSolver  :: Z3.Solver,
    envContext :: Z3.Context,
    envOptimize :: Z3.Optimize
}

-- we need a reader monad here to put the signatures and datatype indices
-- they would not be modified during the synthesis

data EncoderState = EncoderState {
    z3env      :: Z3Env                , -- ^ solver environment
    signatures :: Map Id RSchema       , -- ^ TODO: we may move to GHC structure here
    datatypes  :: Map Int (Set Id)     , -- ^ index of dt in constructor list, index by arity first
    boundTvs   :: Set Id               , -- ^ bounded type variables
    typeSort   :: Sort                 , -- ^ self defined sort with all types
    places     :: [AST]                , -- ^ list of place mappings
    names      :: [Sort]               , -- ^ list of sorted refined by arity
    nameCounter :: Map Id Int          , -- ^ variable name counter
    encoderType :: EncoderType         , -- ^ use normal SMT encoding or refined arity encoding
    okaySet    :: [SType]
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
    let tyNames = Set.toList (dts `Set.union` tvs)
    let dtIdx = foldr addDatatype Map.empty tyNames
    nameList <- mapM createNameSort (Map.toList dtIdx)
    nameListStr <- mapM sortToString nameList
    liftIO $ print nameListStr
    liftIO $ print dtIdx
    put $ st { datatypes = dtIdx, names = nameList }
  where
    addDatatype (name, arity) = Map.insertWith Set.union arity (Set.singleton name)

    createNameSort (arity, nameSet) = do
        name <- mkStringSymbol ("Name" ++ show arity)
        cnstrNames <- mapM mkStringSymbol (Set.toList nameSet)
        cnstrs <- mapM (\x -> mkConstructor x x []) cnstrNames
        mkDatatype name cnstrs

    allDatatypes (FunctionT _ tArg tRet) = allDatatypes tArg `Set.union` allDatatypes tRet
    allDatatypes (ScalarT (DatatypeT id tArgs _) _) = (id, length tArgs) `Set.insert` foldr (Set.union . allDatatypes) Set.empty tArgs
    allDatatypes (ScalarT IntT _) = error "Type Int should be a datatype"
    allDatatypes (ScalarT BoolT _) = error "Type Bool should be a datatype"
    allDatatypes (ScalarT (TypeVarT _ id) _) = Set.empty

dummyType :: Z3Env -> IO Sort
dummyType env = Z3.mkBoolSort (envContext env)

-- | createType: create functions for each datatype as constructors in z3
-- strategy from Nadia, enumeration
createType :: Encoder ()
createType = do
    name <- mkStringSymbol "Type"
    dts <- datatypes <$> get
    tyConstructors <- concatMapM addTypes (Map.toList dts)
    typ <- mkDatatype name tyConstructors
    st <- get
    put $ st { typeSort = typ }
  where
    mkParam name i = do
        sym <- mkStringSymbol (name ++ show i)
        return (sym, Nothing, 0)

    addTypes (kind, nameSet) = mapM (addType kind) (Set.toList nameSet)

    addType kind name = do
        tname <- mkStringSymbol name
        -- assume all the types are unbounded variables
        params <- mapM (mkParam name) [1..kind]
        mkConstructor tname tname params

-- | Another way to encode the type constructors by arity
-- strategy from Ranjit, classify by arity
createAType :: Encoder ()
createAType = do
    name <- mkStringSymbol "Type"
    nameList <- names <$> get
    tyConstructors <- mapM createConstructor [0..(length nameList - 1)]
    typ <- mkDatatype name tyConstructors
    st <- get
    put $ st { typeSort = typ }
  where
    mkParam i = do
        sym <- mkStringSymbol ('t' : show i)
        return (sym, Nothing, 0)

    createConstructor arity = do
        tname <- mkStringSymbol ('C' : show arity)
        params <- mapM mkParam [1..arity]
        cname <- mkStringSymbol ("name" ++ show arity)
        nameList <- names <$> get
        let csort = nameList !! arity
        mkConstructor tname tname ((cname, Just csort, 1):params) -- TODO: what is the sort ref here?

createPlaces :: Int -> Encoder ()
createPlaces l = do
    int <- mkIntSort
    typ <- typeSort <$> get
    parr <- mkArraySort typ int
    pl <- createPlaceAt parr l
    st <- get
    put $ st { places = (places st) ++ [pl] }
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
    let ids = concatMap Set.toList (Map.elems dts)
    idx <- case elemIndex id ids of
             Just i  -> return i
             Nothing -> error $ "mkArg: cannot find such datatype " ++ show (id, length args)
    -- create type application
    mkApp (constructors !! idx) args'
mkArg tvMap t = error $ "mkArg: unsupported argument type " ++ show t

mkIdFunc :: Id -> Int -> Encoder AST
mkIdFunc id arity = do
    nameList <- names <$> get
    let nameSort = nameList !! arity
    nameCnstrs <- getDatatypeSortConstructors nameSort
    dts <- datatypes <$> get
    let idx = Set.findIndex id (Map.findWithDefault Set.empty arity dts)
    mkApp (nameCnstrs !! idx) []

mkAArg :: Map Id AST -> SType -> Encoder AST
mkAArg tvMap (ScalarT (TypeVarT _ id) r) = 
    case Map.lookup id tvMap of
        Just v -> return v
        Nothing -> mkAArg tvMap (ScalarT (DatatypeT id [] []) r)
mkAArg tvMap (ScalarT (DatatypeT id args _) _) = do
    -- create z3 arguments
    args' <- mapM (mkAArg tvMap) args
    -- get the datatype function from constructors
    typ <- typeSort <$> get
    constructors <- getDatatypeSortConstructors typ
    let arity = length args
    idFunc <- mkIdFunc id arity
    mkApp (constructors !! arity) (idFunc : args')
mkAArg tvMap t = error $ "mkAAarg: unsupported argument type " ++ show t

encodeArg :: Map Id AST -> SType -> Encoder AST
encodeArg tvMap t = do
    encoder <- encoderType <$> get
    case encoder of
        Normal -> mkArg tvMap t
        Arity  -> mkAArg tvMap t

createFuncs :: Int -> Encoder ()
createFuncs l = do
    sigs <- signatures <$> get
    mapM_ createFunc [(f, l-1) | f <- Map.toList sigs ]
  where
    createFunc ((id, typ), i) = do
        -- create a boolean variable per transition per time
        f <- mkStringSymbol (id ++ show i)
        bool <- mkBoolSort
        fc <- mkConst f bool

        -- create varaibles for each type variable in the current signature
        let tvs = boundVarsOf typ
        bounded <- boundTvs <$> get
        tm <- mkBoundTm (tvs ++ Set.toList bounded)

        uno <- mkIntNum 1
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
        pAfter <- foldrM (uncurry (applyArgToken tm i)) (pls !! i) (Map.toList finalMap)
        changeP <- mkEq (pls !! (i+1)) pAfter
        -- both arguments and return type should be in our okay set
        okayArgs <- mapM (okayArg tm . shape) (ret : args)
        -- there exists some type variables to satisfy the requirements
        canFire <- mkAnd (changeP:hasArgs ++ okayArgs)
        tvSymbols <- mapM mkIntSymbol [(length tvs - 1),(length tvs -2)..0]
        styp <- typeSort <$> get
        let tvTypes = replicate (length tvs) styp
        fired <- if length tvSymbols > 0 
                   then mkExists [] tvSymbols tvTypes canFire >>= mkImplies fc
                   else mkImplies fc canFire
        assert fired

    mkBoundTm tvs = foldrM addBoundTv Map.empty (zip tvs [0,1..])

    addBoundTv (tv, i) m = do
        typ <- typeSort <$> get
        x <- mkBound i typ
        return (Map.insert tv x m)

    hasArgAt tvMap i arg cnt = do
        pls <- places <$> get
        varg <- encodeArg tvMap arg
        carg <- mkSelect (pls !! i) varg
        consumed <- mkIntNum (-cnt)
        mkGe carg consumed

    okayArg tvMap arg = do
        varg <- encodeArg tvMap arg
        styp <- typeSort <$> get
        typSet <- mkSetSort styp
        okaySet <- mkStringSymbol "okaySet" >>= flip mkConst typSet
        mkSetMember varg okaySet

    decArgToken arg mBefore = Map.insertWith (+) (shape arg) (-1) mBefore

    applyArgToken tvMap i arg cnt pBefore = do
        if cnt == 0
          then return pBefore
          else do
            pls <- places <$> get
            varg <- encodeArg tvMap arg
            tBefore <- mkSelect (pls !! i) varg
            consumed <- mkIntNum cnt
            tAfter <- mkAdd [tBefore, consumed]
            mkStore pBefore varg tAfter

-- | set initial state for petri net solving
-- take a list of bounded type variables@tvs, argument types@args
-- return a new state
setInitial :: [Id] -> [RType] -> Encoder ()
setInitial tvs args = do
    typ <- typeSort <$> get
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
        vArg <- encodeArg tm (shape tArg)
        vCnt <- mkIntNum cnt
        mkStore vArr vArg vCnt

-- | set final state for petri net solving
-- take a list of bounded type variables@tvs and return type@tRet
setFinal :: [Id] -> RType -> Int -> Encoder ()
setFinal tvs tRet i = do
    typ <- typeSort <$> get
    let tm = Map.empty

    -- constraint: only one token in the return type and all the others have none
    int <- mkIntSort
    parr <- mkArraySort typ int
    zero <- mkIntNum 0
    emptyArr <- mkConstArray typ zero
    vRet <- encodeArg tm (shape tRet)
    uno <- mkIntNum 1
    assignedArr <- mkStore emptyArr vRet uno

    pls <- places <$> get
    final <- mkEq (pls !! i) assignedArr
    assert final

okayTypes :: Encoder ()
okayTypes = do
    typs <- okaySet <$> get
    vtyps <- mapM (encodeArg Map.empty) typs
    typ <- typeSort <$> get
    typSet <- mkSetSort typ
    empty <- mkEmptySet typ
    contains <- foldM mkSetAdd empty vtyps
    okay <- mkStringSymbol "okaySet" >>= flip mkConst typSet
    mkEq okay contains >>= assert

-- | set constraints on what kind of types can have positive tokens
okayType :: Int -> Encoder ()
okayType i = do
    tSymbol <- mkIntSymbol 0
    styp <- typeSort <$> get
    t <- mkBound 0 styp
    typs <- okaySet <$> get
    -- if the type is not in our okaySet, it always has zero tokens
    notContains <- mapM (notInSet t) typs >>= mkAnd
    noToken <- hasNothing t
    okayRule <- mkImplies notContains noToken
    okay <- mkForall [] [tSymbol] [styp] okayRule
    assert okay
  where
    notInSet t typ = do
        vTyp <- encodeArg Map.empty typ
        mkEq t vTyp >>= mkNot

    hasNothing t = do
        zero <- mkIntNum 0
        pls <- places <$> get
        tokens <- mkSelect (pls !! i) t
        mkEq tokens zero


encode :: [Id] -> [RType] -> RType -> Int -> Encoder ()
encode tvs args tRet i = do
    -- create list of place arrays
    createPlaces i
    -- create constraints
    createFuncs i
    -- set initial state
    setInitial tvs args

    -- optimizePush
    push
    liftIO $ print $ "trying length of " ++ show i
    -- set final state
    setFinal tvs tRet i
    -- at least one transition is fired at each time
    mapM_ atExactlyOne [0..(i-1)]

    solverStr <- solverToString
    liftIO $ putStrLn solverStr
    -- check by the solver
    liftIO $ putStrLn "start checking"
    res <- solverCheck
    case res of 
        Sat -> do
            model <- solverGetModel
            modelStr <- modelToString model
            liftIO $ putStrLn modelStr
        _ -> do
            liftIO $ putStrLn "Here UNSAT"
            when (i < 3) $ do
                pop 1
                encode tvs args tRet (i+1)
  where
    -- fire only one transition at each timestamp
    atExactlyOne t = do
        bool <- mkBoolSort
        sigs <- signatures <$> get
        fs <- mapM (\name -> mkStringSymbol (name ++ show t)) (Map.keys sigs)
        fc <- mapM (\sym -> mkConst sym bool) fs
        -- at least one transition is fired at the current timestamp
        mkOr fc >>= assert
        -- at most one transition is fired at the current timestamp
        -- mapM_ (fireAt fc) [0..(length fc - 1)]

    fireAt fc i = do
        let selected = fc !! i
        let other = deleteAt i fc
        mkOr other >>= mkNot >>= mkImplies selected >>= assert

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
    et <- encoderType <$> get
    case et of
      Normal -> createType
      Arity  -> createAType
    createPlaces 0
    okayTypes
    encode tvs args ret 1
