{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module PetriNet.PNSolver where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics
import Control.Lens
import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Data (Data)
import qualified Data.Char as Char
import Data.Either hiding (fromLeft, fromRight)
import Data.List.Extra
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos
import Data.Serialize (Serialize)
import Data.Aeson (ToJSON, genericToEncoding, defaultOptions)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import Debug.Trace
import Language.Haskell.Exts.Parser (parseExp, ParseResult(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Z3.Monad as Z3
import System.CPUTime
import Text.Printf

import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Program
import Synquid.Type
import Synquid.Logic
import Synquid.Util
import Synquid.Error
import Synquid.Pretty
import PetriNet.AbstractType
import PetriNet.PNBuilder
import PetriNet.PNEncoder
import PetriNet.Encoder
import PetriNet.PNEncoder
import PetriNet.GHCChecker
import HooglePlus.CodeFormer
import Database.Convert
import Database.Generate

data PathSolver = 
    SATSolver
  | SMTSolver
  deriving(Data, Show, Eq)

data RefineStrategy =
    NoRefine
  | AbstractRefinement
  deriving(Data, Show, Eq)

data SolverState = SolverState {
    _nameCounter :: Map Id Int,  -- name map for generating fresh names (type variables, parameters)
    _typeAssignment :: Map Id SType,  -- current type assignment for each type variable
    _typingError :: (SType, SType), -- typing error message, represented by the expected type and actual type
    _abstractionTree :: AbstractionTree,
    _isChecked :: Bool, -- is the current state check passed
    _currentSolutions :: [RProgram], -- type checked solutions
    _currentLoc :: Int, -- current solution depth
    _currentSigs :: Map Id AbstractSkeleton, -- current type signature groups
    _functionMap :: HashMap Id FunctionCode,
    _targetType :: Id,
    _sourceTypes :: [Id],
    _paramNames :: [Id],
    _refineStrategy :: RefineStrategy,
    _groupMap :: Map Id [Id], -- mapping from group id to list of function names
    _type2group :: Map AbstractSkeleton [Id], -- mapping from abstract type to group ids
    _logLevel :: Int -- temporary for log level
} deriving(Eq)

emptySolverState = SolverState {
    _nameCounter = Map.empty,
    _typeAssignment = Map.empty,
    _typingError = (AnyT, AnyT),
    _abstractionTree = ALeaf (AExclusion Set.empty),
    _isChecked = True,
    _currentSolutions = [],
    _currentLoc = 1,
    _currentSigs = Map.empty,
    _functionMap = HashMap.empty,
    _targetType = "",
    _sourceTypes = [],
    _paramNames = [],
    _refineStrategy = NoRefine,
    _groupMap = Map.empty,
    _type2group = Map.empty,
    _logLevel = 0
}

makeLenses ''SolverState

type PNSolver m = StateT SolverState m

abstractParamList :: AbstractSkeleton -> [AbstractSkeleton]
abstractParamList t@(ADatatypeT _ _) = [t]
abstractParamList t@(AExclusion _)   = [t]
abstractParamList (AFunctionT tArg tFun) = 
    case tFun of
        ADatatypeT _ _  -> [tArg]
        AExclusion _    -> [tArg]
        ATypeVarT  _    -> [tArg]
        _               -> (tArg) : (abstractParamList tFun)
abstractParamList t = error $ "Unexpected type " ++ show t

lastAbstractType :: AbstractSkeleton -> AbstractSkeleton
lastAbstractType (AFunctionT tArg tFun) = lastAbstractType tFun
lastAbstractType t                      = t

encodeFunction :: Id -> AbstractSkeleton -> FunctionCode
encodeFunction id t@(AFunctionT tArg tRet) = FunctionCode id hoParams params (show $ lastAbstractType t)
  where
    base = (0, [])
    hoFun x (i, a) = (i+1, (encodeFunction ("f" ++ show i) x) : a)
    paramFun x (i, a) = if isAFunctionT x then (i+1, ("f" ++ show i):a) else (i, (show x):a)
    (_, hoParams) = foldr hoFun base $ filter isAFunctionT (abstractParamList t)
    (_, params) = foldr paramFun base (abstractParamList t)
encodeFunction id t                        = FunctionCode id [] [] (show t)

type ClassConstraint = [(Id, [Id])]

freshId :: (MonadIO m) => Id -> PNSolver m Id
freshId prefix = do
    indices <- flip (^.) nameCounter <$> get
    let idx = Map.findWithDefault 0 prefix indices
    modify (over nameCounter $ Map.insert prefix (idx+1))
    return $ prefix ++ "_" ++ show idx

-- | Replace all bound type variables with fresh free variables
freshType :: (MonadIO m) => RSchema -> PNSolver m RType
freshType sch = freshType' Map.empty [] sch
  where
    freshType' subst constraints (ForallT a sch) = do
        a' <- freshId "A"
        freshType' (Map.insert a (vart a' ftrue) subst) (a':constraints) sch    
    freshType' subst constraints (Monotype t) = return (typeSubstitute subst t)

{- 
constructType :: Environment -> AbstractionSemantic -> Id -> [AbstractSkeleton]
constructType env abstraction key 
    | key `Map.notMember` abstraction && currDt == "" = [AExclusion Set.empty]
    | key `Map.notMember` abstraction && isDt = [ADatatypeT currDt $ replicate dtParams (AExclusion Set.empty)]
    | key `Map.notMember` abstraction = [ATypeVarT currDt, AExclusion (Set.singleton currDt)] -- this is bounded type variables
    | currDt == "" = nubOrd $ (AExclusion allIds) : concatMap (constructType env abstraction . (++) (key ++ ",")) (Set.toList allIds)
    | isDt = nubOrd $ map (ADatatypeT currDt) $ listOfN dtParams $ (AExclusion allIds) : concatMap (constructType env abstraction . (++) (key ++ ",")) (Set.toList allIds)
    | otherwise = nubOrd $ [ATypeVarT currDt, AExclusion (Set.singleton currDt)] -- this is bounded type variables
  where
    currDt = if null (splitBy ',' key) 
                then "" 
                else if Map.member (last $ splitBy ',' key) (env ^. datatypes) || Char.isLower (head $ last $ splitBy ',' key)
                    then last $ splitBy ',' key
                    else ""
    allIds = Map.findWithDefault Set.empty key abstraction
    isDt = Char.isUpper $ head currDt
    dtParams = case Map.lookup currDt (env ^. datatypes) of
        Just def -> length (def ^. typeParams)
        Nothing  -> error $ "constructType: cannot find datatype " ++ currDt ++ " in the environment\n" ++ "here are the datatypes: " ++ (show $ Map.keys $ env ^. datatypes)
    
    listOfN 0 _  = []
    listOfN 1 xs = map (: []) xs
    listOfN n xs = [x:y | x <- xs, y <- listOfN (n-1) xs]
-}

instantiate :: (MonadIO m) => Environment -> [(Id, AbstractSkeleton)] -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs = 
    Map.fromList <$> instantiate' sigs (filter (not . hasAbstractVar . snd) sigs)
  where 
    removeSuffix id ty = (removeLast '_' id, ty)
    
    instantiate' sigs sigsAcc = do
        st <- get
        let typs = leafTypes (st ^. abstractionTree)
        writeLog 3 $ text "Current abstract types:" <+> pretty typs 
        sigs' <- foldM (\acc -> (<$>) ((++) acc) . uncurry (instantiateWith env typs)) [] sigs
        return $ nubOrdOn (uncurry removeSuffix) (sigsAcc ++ sigs')

instantiateWith :: (MonadIO m) => Environment -> [AbstractSkeleton] -> Id -> AbstractSkeleton -> PNSolver m [(Id, AbstractSkeleton)]
instantiateWith env typs id sk = do
    let vars = Set.toList $ allAbstractVar sk
    let multiSubsts = map (zip vars) $ multiPermutation (length vars) typs
    let substedSymbols = map (foldr (\(id, t) acc -> abstractSubstitute (env ^. boundTypeVars) id t acc) sk) multiSubsts
    st <- get
    let refinedSymbols = nubOrd (concatMap (cutoff (st ^. abstractionTree)) substedSymbols)
    foldrM (\t accMap -> do
        newId <- newSymbolName id
        return $ (newId, t):accMap) [] refinedSymbols
    where

        newSymbolName prefix = do
            indices <- flip (^.) nameCounter <$> get
            let idx = Map.findWithDefault 0 prefix indices
            modify (over nameCounter $ Map.insert prefix (idx+1))
            return $ prefix ++ "_" ++ show idx

-- | group all the signatures in @sigs@ by their abstract signature
-- i.e. functions with the same signature becomes one transition in petri net
groupSigs :: (MonadIO m) => Map Id AbstractSkeleton -> PNSolver m ()
groupSigs sigMap = do
    let sigs = Map.toList sigMap
    mapM_ createGroup (group (sortOn snd sigs))

createGroup :: MonadIO m => [(Id, AbstractSkeleton)] -> PNSolver m Id
createGroup sigList = do
    let key = snd (head sigList)
    let value = map fst sigList
    -- create a new group name and add the signatures into them
    gm <- view groupMap <$> get
    let groupIdx = Map.size gm
    let name = "group" ++ show groupIdx
    -- store the group information for code former
    modify $ over groupMap $ Map.insert name value
    -- insert the group name and its type into current sigsMap
    modify $ over currentSigs $ Map.insert name key
    -- store the used abstract types and their groups into mapping
    let addGroup k gid = Map.insertWith (++) k [gid]
    let includedTyps = decompose key
    mapM_ (\t -> modify $ over type2group (addGroup t name)) includedTyps
    return name

data SplitInfo = SplitInfo {
    oldPlace :: AbstractSkeleton,
    newPlace :: [AbstractSkeleton],
    splitedGroup :: [(Id, [Id])]
} deriving (Eq, Ord)

splitGroup :: MonadIO m => Environment -> Id -> SplitInfo -> PNSolver m SplitInfo
splitGroup env gid info = do
    gm <- view groupMap <$> get
    sigs <- view currentSigs <$> get
    let typ = groupSig sigs
    let ids = groupedIds gm
    let splitedTyp = oldPlace info
    let newTyps = newPlace info
    -- step 1: unify the type with refined abstract type and get new signature
    unifyRes <- mapM (\nt -> firstUnified typ nt ids) newTyps
    let unifiedTyps = concatMap fromJust (filter isJust unifyRes)
    -- step 2: classify the signatures into new groups
    let newGroups = groupBy (\t1 t2 -> snd t1 == snd t2) (sortOn snd unifiedTyps)
    -- step 3: pack the information into SplitInfo for incremental encoding
    newIds <- mapM createGroup newGroups
    return (info { splitedGroup = (gid, newIds) : (splitedGroup info) })
  where
    groupedIds gm = case Map.lookup gid gm of
                        Just ids -> ids
                        Nothing  -> error $ printf "cannot find group %s in group map" gid

    groupSig sigs = case Map.lookup gid sigs of
                        Just sig -> sig
                        Nothing -> error $ printf "cannot find group %s in sig map" gid

    matchTyps target (AFunctionT tArg tRet) (AFunctionT tArg' tRet') =
        matchTyps target tArg tArg' ++ matchTyps target tRet tRet'
    matchTyps target absTyp polyTyp
        | target == absTyp = [polyTyp]
        | otherwise = []

    firstUnified typ newTyp [] = return Nothing
    firstUnified typ newTyp (id:ids) = do
        polyTyp <- toAbstractType . shape <$> findSymbol env (removeLast '_' id)
        let typs = matchTyps (oldPlace info) typ polyTyp
        -- pass the current abstract hierarchy into the unifier
        let unifiers = map (unifier (env ^. boundTypeVars) newTyp) typs
        let checkFail = filter isNothing unifiers
        if null checkFail
           then do
               let unpackedUnifiers = map fromJust unifiers
               let subst = Map.filter (not . Set.null) (checkConsistent unpackedUnifiers)
               if Map.null subst
                  then firstUnified typ newTyp ids
                  else do
                      semantic <- view abstractionTree <$> get
                      let ts = applySubst (env ^. boundTypeVars) subst polyTyp
                      let ts' = nubOrd (concatMap (cutoff semantic) ts)
                      ids <- mapM (\_ -> freshId id) ts'
                      return (Just (zip ids ts'))
           else firstUnified typ newTyp ids

    -- TODO check this format with the unification algorithm
    -- assume substitution has the format of Map Id (Set AbstractSkeleton)
    checkConsistent [] = error "No corresponding types in the current list"
    checkConsistent (unif:unifiers) = foldr checkIntersection unif unifiers

    checkIntersection = Map.intersectionWith Set.intersection

cutoff :: AbstractionTree -> AbstractSkeleton -> [AbstractSkeleton]
cutoff semantic (ATypeVarT id) = [rightmostType semantic]
cutoff (ALeaf t) (ADatatypeT id tArgs) = [t]
cutoff (ANode _ lt rt) typ@(ADatatypeT {}) | isSubtypeOf typ (valueType lt) = cutoff lt typ
cutoff (ANode _ lt rt) typ@(ADatatypeT {}) | isSubtypeOf typ (valueType rt) = cutoff rt typ
cutoff (ANode t lt rt) typ@(ADatatypeT {}) | t == typ = [typ] -- there exists some temporarily imprecise types
cutoff semantic (AFunctionT tArg tRet) = 
    [ AFunctionT a r | a <- args, r <- rets, isJust (isConsistent (AFunctionT tArg tRet) (AFunctionT a r)) ]
  where
    args = cutoff semantic tArg
    rets = cutoff semantic tRet

    isMapConsistent m1 m2 = foldr (\(k,v) -> (&&) (Map.findWithDefault v k m2 == v)) True (Map.toList m1)
    isConsistent t t'@(ATypeVarT {}) = Just (Map.singleton t t')
    isConsistent t t'@(ADatatypeT {}) = Just (Map.singleton t t')
    isConsistent t t'@(AExclusion {}) = Just (Map.singleton t t')
    isConsistent (AFunctionT tArg tRet) (AFunctionT arg ret) = 
        let argMap = isConsistent tArg arg
            retMap = isConsistent tRet ret
        in if isJust argMap && isJust retMap && isMapConsistent (fromJust argMap) (fromJust retMap)
              then Just (Map.union (fromJust argMap) (fromJust retMap))
              else Nothing
cutoff semantic t@(AExclusion _) = [rightmostType semantic]
cutoff semantic t = leafTypes (closestTree semantic t)

updateSemantic :: AbstractionTree -> AbstractSkeleton -> AbstractionTree
updateSemantic semantic (ATypeVarT id) = semantic
updateSemantic semantic@(ALeaf t) typ@(ADatatypeT {}) | t == typ = semantic
updateSemantic semantic@(ALeaf t@(ADatatypeT {})) typ@(ADatatypeT {}) =
    ANode t (ALeaf typ) (ALeaf (typeDifference t typ))
updateSemantic semantic@(ALeaf t) typ@(ADatatypeT id tArgs) | t /= typ = 
    updateSemantic semantic' typ
  where
    emptyArgs = map fillAny tArgs
    typ' = ADatatypeT id emptyArgs
    semantic' = ANode t (ALeaf typ') (ALeaf (typeDifference t typ'))
updateSemantic semantic@(ANode t lt rt) typ@(ADatatypeT id tArgs)
  | t == typ = semantic
  | isSubtypeOf typ t && isSubtypeOf typ (valueType lt) = ANode t (updateSemantic lt typ) rt
  | isSubtypeOf typ t && isSubtypeOf typ (valueType rt) = ANode t lt (updateSemantic rt typ)
  | otherwise = error (printf "%s is not a subtype of %s, not a subtype of %s, not a subtype of %s, we should not reach here" (show typ) (show t) (show (valueType lt)) (show (valueType rt)))
updateSemantic semantic (AFunctionT tArg tRet) = semantic''
  where
    semantic' = updateSemantic semantic tArg
    semantic'' = updateSemantic semantic' tRet
updateSemantic semantic (AExclusion _) = semantic

type SplitMsg = (AbstractSkeleton, [AbstractSkeleton])

distinguish :: MonadIO m => Environment -> SType -> SType -> PNSolver m (Maybe SplitMsg)
distinguish env t1 t2 = do
    tass <- view typeAssignment <$> get
    semantic <- view abstractionTree <$> get
    let t1' = stypeSubstitute tass t1
    let t2' = stypeSubstitute tass t2
    let at1:_ = cutoff semantic (toAbstractType t1')
    let at2:_ = cutoff semantic (toAbstractType t2')
    -- only try to get split information when the two types have 
    -- different abstract representations in the current abstraction level
    if at1 /= at2
       then return Nothing
       else case distinguish' env t1' t2' of
              [] -> return Nothing
              ts -> return (Just (at1, ts))

distinguish' :: Environment -> SType -> SType -> [AbstractSkeleton]
distinguish' env AnyT _ = []
distinguish' env _ AnyT = []
distinguish' env (ScalarT (DatatypeT id args _) _) (ScalarT (DatatypeT id' _ _) _) | id /= id' = 
    [ADatatypeT id emptyArgs, ADatatypeT id' emptyArgs]
  where
    emptyArgs = map fillAny args
distinguish' env (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id == id' = 
    case disArgs tArgs tArgs' of
        []    -> []
        diffs -> map (ADatatypeT id) diffs
  where
    -- the two datatypes should have the same length of argument list
    disArgs [] [] = []
    disArgs (arg:args) (arg':args') = 
        case distinguish' env arg arg' of
          [] -> case disArgs args args' of
                  []    -> []
                  diffs -> map ((:) (toAbstractType arg)) diffs
          diffs -> map (:(map fillAny args)) diffs
-- TODO: need change to support higher order functions
distinguish' env (ScalarT (TypeVarT _ id) _) (ScalarT (TypeVarT _ id') _) | id == id' = []
{- has unbounded type variable, view it as AnyT, this should not happen
-- TODO: remove these two rules?
distinguish' env (ScalarT (TypeVarT _ id) _) t | not (isBound env id) = []
distinguish' env t (ScalarT (TypeVarT _ id) _) | not (isBound env id) = []
-}
-- has bounded type variable
distinguish' env (ScalarT (TypeVarT {}) _) (ScalarT (TypeVarT {}) _) = []
distinguish' env (ScalarT (TypeVarT _ _) _) (ScalarT (DatatypeT id args _) _) = 
    [AExclusion (Set.singleton id), ADatatypeT id (map fillAny args)]
distinguish' env t tv@(ScalarT (TypeVarT _ _) _) = distinguish' env tv t
distinguish' env (FunctionT _ tArg tRes) (FunctionT _ tArg' tRes') = 
    case distinguish' env tArg tArg' of
      []  -> distinguish' env tRes tRes'
      res -> res
distinguish' env t1 t2 = error (printf "unhandled case for distinguish %s and %s" (show t1) (show t2))

findSymbol :: MonadIO m => Environment -> Id -> PNSolver m RType
findSymbol env sym = 
    case lookupSymbol sym 0 env of
        Nothing -> do
            case lookupSymbol ("(" ++ sym ++ ")") 0 env of
                Nothing -> do
                    modify $ set isChecked False
                    writeLog 2 $ text "cannot find symbol" <+> text sym <+> text "in the current environment"
                    return AnyT
                Just sch -> freshType sch
        Just sch -> freshType sch

topDownCheck :: (MonadIO m) => Environment -> SType -> UProgram -> PNSolver m RProgram
topDownCheck env typ p@(Program (PSymbol sym) _) = do
    -- lookup the symbol type in current scope
    writeLog 3 $ text "Checking type for" <+> pretty p
    -- liftIO $ print (allSymbols env)
    t <- findSymbol env sym
    -- solve the type constraint t == typ
    writeLog 3 $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
    solveTypeConstraint env typ (shape t)
    ifM (view isChecked <$> get) 
        (return $ Program (PSymbol sym) t) 
        (return $ Program (PSymbol sym) (addTrue typ)) 
topDownCheck env typ p@(Program (PApp pFun pArg) _) = do
    -- first check the function type with @AnyT@ as parameters and @typ@ as returns
    writeLog 3 $ text "Checking type for" <+> pretty p
    fun <- topDownCheck env (FunctionT "x" AnyT typ) pFun
    ifM (view isChecked <$> get)
        (checkArgumentType fun)
        (return $ Program (PApp fun uHole) (addTrue typ))
  where
    checkArgumentType f = do -- otherwise continue check for the argument type
        let FunctionT _ tArg tRet = typeOf f
        arg <- topDownCheck env (shape tArg) pArg
        ifM (view isChecked <$> get)
            (return $ Program (PApp f arg) tRet)
            (return $ Program (PApp f arg) (addTrue typ)) -- if check fails
topDownCheck env typ@(FunctionT _ tArg tRet) p@(Program (PFun x body) _) = do
    writeLog 3 $ text "Checking type for" <+> pretty p
    body' <- topDownCheck (addVariable x (addTrue tArg) env) tRet body
    ifM (view isChecked <$> get)
        (return $ Program (PFun x body') (FunctionT x (addTrue tArg) (typeOf body')))
        (return $ Program (PFun x body') (addTrue typ))

bottomUpCheck :: (MonadIO m) => Environment -> RProgram -> PNSolver m (Either RProgram SplitMsg) 
bottomUpCheck env p@(Program PHole _) = return $ Left p
bottomUpCheck env p@(Program (PSymbol sym) typ) | isFunctionType typ = do
    t <- findSymbol env sym
    return (Left (Program (PSymbol sym) t))
bottomUpCheck env p@(Program (PSymbol sym) typ) | otherwise = do
    -- lookup the symbol type in current scope
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    t <- findSymbol env sym
    writeLog 3 $ text "Distinguishing" <+> pretty (shape typ) <+> text "and" <+> pretty (shape t)
    tass <- view typeAssignment <$> get
    maybeSplit <- distinguish env (shape typ) (shape t)
    case maybeSplit of
        Nothing -> return $ Left (Program (PSymbol sym) t)
        Just msg -> return $ Right msg
bottomUpCheck env (Program (PApp pFun pArg) typ) = do
    fun <- bottomUpCheck env pFun
    case fun of
        Right msg -> return (Right msg)
        Left f -> checkArg f
  where
    checkArg f = do
        let FunctionT _ tArg tRet = typeOf f
        arg <- bottomUpCheck env pArg
        case arg of
            Right msg -> return (Right msg)
            Left a -> do
                writeLog 3 $ text "Solving constraint for" <+> pretty a <+> pretty (shape $ typeOf a) <+> text "==" <+> pretty (shape tArg)
                solveTypeConstraint env (shape $ typeOf a) (shape tArg) 
                distinguishFunc f a tArg tRet

    distinguishFunc f a tArg tRet = do
        tass <- view typeAssignment <$> get
        writeLog 3 $ text "Distinguishing" <+> pretty (shape $ typeOf a) <+> text "and" <+> pretty (shape tArg)
        splitArg <- distinguish env (shape (typeOf a)) (shape tArg)
        case splitArg of
            Nothing -> do
                writeLog 3 $ text "Distinguishing" <+> pretty (shape typ) <+> text "and" <+> pretty (shape tRet)
                splitRet <- distinguish env (shape typ) (shape tRet)
                case splitRet of
                    Nothing -> ifM (view isChecked <$> get)
                                   (return $ Left (Program (PApp f a) tRet))
                                   (return $ Left (Program PHole tRet))
                    Just msg -> return $ Right msg
            Just msg -> return $ Right msg

bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    body' <- bottomUpCheck (addVariable x (addTrue tArg) env) body
    case body' of
        Right msg -> return (Right msg)
        Left b -> return $ Left (Program (PFun x b) (FunctionT x tArg (typeOf b)))

solveTypeConstraint :: (MonadIO m) => Environment -> SType -> SType -> PNSolver m ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | id == id' = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | isBound env id && isBound env id' =
    modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | isBound env id = do
    st <- get
    if id' `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty tv
            solveTypeConstraint env tv typ
        else unify env id' tv
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) = do
    st <- get 
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty tv'
            solveTypeConstraint env typ tv'
        else if id' `Map.member` (st ^. typeAssignment)
            then do
                let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
                writeLog 3 $ text "Solving constraint" <+> pretty tv <+> "==" <+> pretty typ
                solveTypeConstraint env tv typ
            else do
                unify env id tv'
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty t
            solveTypeConstraint env typ t
        else do
            unify env id t
solveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 $ text "Solving constraint" <+> pretty tArg <+> "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    st <- get
    when (st ^. isChecked) (do
        writeLog 3 $ text "Solving constraint" <+> pretty tRet <+> "==" <+> pretty tRet'
        solveTypeConstraint env tRet tRet')
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id /= id' = do
    modify $ set isChecked False
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id == id' = do
    solveTypeConstraint' env tArgs tArgs'
  where
    solveTypeConstraint' _ []  [] = return ()
    solveTypeConstraint' env (ty:tys) (ty':tys') = do
        writeLog 3 $ text "Solving constraint" <+> pretty ty <+> "==" <+> pretty ty'
        solveTypeConstraint env ty ty'
        checked <- view isChecked <$> get
        -- if the checking between ty and ty' succeeds, proceed to others
        when (checked) (solveTypeConstraint' env tys tys')
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
solveTypeConstraint env t1 t2 = error $ "unknown types " ++ show t1 ++ " or " ++ show t2

unify :: (MonadIO m) => Environment -> Id -> SType -> PNSolver m ()
unify env v t = modify $ over typeAssignment (Map.insert v t)

checkType :: (MonadIO m) => Environment -> RType -> UProgram -> PNSolver m (Either RProgram RProgram)
checkType env typ p = do
    writeLog 1 $ text "Find program" <+> pretty p
    modify $ set isChecked True
    top <- topDownCheck env (shape typ) p
    writeLog 3 $ text "top down checking get program" <+> pretty top
    ifM (view isChecked <$> get)
        (return $ Left top)
        (return $ Right top)
            {-
        (do
            rs <- view refineStrategy <$> get
            case rs of
                NoRefine -> return Nothing
                AbstractRefinement -> do
                    info <- refineSemantic env top
                    -- TODO pass the split info to encoder for incremental encoding
                    return Nothing
        )
-}

refineSemantic :: MonadIO m => Environment -> RProgram -> PNSolver m SplitInfo
refineSemantic env top = do
    modify $ set isChecked True
    checkRes <- bottomUpCheck env top
    case checkRes of
        Left p -> error "bottom up check passed but top down check failed"
        Right (t, ts) -> do
            semantic <- view abstractionTree <$> get
            let t':_ = cutoff semantic t
            let semantic' = foldr (flip updateSemantic) semantic ts
            writeLog 3 $ text $ printf "before update semantic, %s is splited into %s" (show t') (show ts)
            writeLog 3 $ text $ printf "new semantics is %s" (show semantic')
            modify $ set abstractionTree semantic'
            let ts' = map (head . cutoff semantic') ts
            writeLog 3 $ text $ printf "%s is splited into %s" (show t') (show ts')
            t2g <- view type2group <$> get
            let groupIds = Map.findWithDefault [] t' t2g
            let splitNode = SplitInfo t' ts' []
            foldrM (splitGroup env) splitNode groupIds

-- | wrap some action with time measuring and print out the execution time
withTime :: MonadIO m => String -> PNSolver m a -> PNSolver m a
withTime desc f = do
    start <- liftIO getCPUTime
    res <- f
    end <- liftIO getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    let time = printf "%s time: %0.3f sec\n" desc (diff :: Double)
    writeLog 3 $ text time
    return res

initNet :: MonadIO m => Environment -> PNSolver m PetriNet
initNet env = do
    let binds = env ^. boundTypeVars
    abstraction <- view abstractionTree <$> get
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    absSymbols <- mapM (uncurry abstractSymbol) (Map.toList (allSymbols env))
    sigs <- withTime "instantiate sigs" $ instantiate env absSymbols
    -- first order arguments are tokens but not transitions in petri net
    let usefulSigs = foldr Map.delete sigs (Map.keys foArgs)
    -- group the signatures into bins
    groupSigs usefulSigs
    mapM_ addEncodedFunction (Map.toList usefulSigs)
    groups <- view currentSigs <$> get
    let symbols = map (uncurry encodeFunction) (Map.toList groups)
    -- symbols <- mapM addEncodedFunction (Map.toList usefulSigs)
    let srcTypes = map ( show
                       . head 
                       . cutoff abstraction
                       . toAbstractType
                       . shape 
                       . toMonotype) $ Map.elems foArgs
    liftIO $ print srcTypes
    modify $ set sourceTypes srcTypes
    withTime "petri net construction" (return (buildPetriNet symbols srcTypes))
  where
    abstractSymbol id sch = do
        t <- freshType sch
        let absTy = toAbstractType (shape t) -- abstract binds abstraction (shape t)
        return (id, absTy)

    addEncodedFunction (id, f) = do
        let ef = encodeFunction id f
        modify $ over functionMap (HashMap.insert id ef)
        return ef

resetEncoder :: (MonadIO m) => Environment -> RType -> PetriNet -> PNSolver m EncodeState
resetEncoder env dst net = do
    let binds = env ^. boundTypeVars
    abstraction <- view abstractionTree <$> get
    let tgt = show (head (cutoff abstraction (toAbstractType (shape dst))))
    liftIO $ print tgt
    modify $ set targetType tgt
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    modify $ set paramNames $ Map.keys foArgs
    srcTypes <- view sourceTypes <$> get
    writeLog 2 $ text "parameter types are" <+> pretty srcTypes
    writeLog 2 $ text "return type is" <+> pretty tgt
    loc <- view currentLoc <$> get
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    liftIO $ encoderInit net loc hoArgs srcTypes tgt

findPath :: (MonadIO m) => Environment -> RType -> PetriNet -> EncodeState -> PNSolver m (CodePieces, EncodeState)
findPath env dst net st = do
    (res, st') <- withTime "solver check" (liftIO (encoderSolve st))
    case res of
        [] -> do
            currSt <- get
            when (currSt ^. currentLoc >= 3) (error "cannot find a path")
            modify $ set currentLoc ((currSt ^. currentLoc) + 1)
            st'' <- resetEncoder env dst net
            findPath env dst net st''
        _  -> do
            fm <- view functionMap <$> get
            src <- view sourceTypes <$> get
            args <- view paramNames <$> get
            groups <- view groupMap <$> get
            let sortedRes = sortOn snd res
            let transNames = map (transitionId . fst) sortedRes
            let usefulTrans = filter skipEntry transNames
            let sigNames = map removeSuffix usefulTrans
            -- let sigs = map (findFunction fm) sigNames
            let sigs = combinations (map (findFunctions fm groups) sigNames)
            let initialFormer = FormerState 0 HashMap.empty
            code <- withTime "code former" -- (generateCode initialFormer src args sigs)
                  $ mapM (generateCode initialFormer src args) sigs
            return (Set.unions code, st')
            -- return (code, st')
  where
    findFunctions fm groups name = 
        case Map.lookup name groups of
            Just g -> map (findFunction fm) g
            Nothing -> error $ "cannot find function group " ++ name

    findFunction fm name =
        case HashMap.lookup name fm of
            Just fc -> fc
            Nothing -> error $ "cannot find function name " ++ name

    combinations []    = []
    combinations [x]   = [x]
    combinations (h:t) = [ hh:tt | hh <- h, tt <- combinations t ]

    generateCode initialFormer src args sigs = 
        liftIO (evalStateT (generateProgram sigs src args) initialFormer)
    skipEntry = not . isInfixOf "|entry"
    removeSuffix = removeLast '|'

findProgram :: (MonadIO m) => Environment -> RType -> PetriNet -> EncodeState -> PNSolver m (RProgram, EncodeState)
findProgram env dst net st = do
    liftIO $ putStrLn "calling findProgram"
    (codeResult, st') <- findPath env dst net st
    oldSemantic <- view abstractionTree <$> get
    liftIO $ print codeResult
    checkResult <- mapM parseAndCheck $ Set.toList codeResult
    let codes = lefts checkResult
    let errors = rights checkResult
    if null codes
        then findNextSolution st' oldSemantic (head errors)
        else checkSolution st' codes
  where
    parseAndCheck code = do
        let prog = case parseExp code of
                       ParseOk exp -> toSynquidProgram exp
                       ParseFailed loc err -> error err
        checkType env dst prog

    findNextSolution st oldSemantic errorProg = do
        rs <- view refineStrategy <$> get
        case rs of
            NoRefine -> findProgram env dst net st
            AbstractRefinement -> do
                refineSemantic env errorProg
                newSemantic <- view abstractionTree <$> get
                refine st oldSemantic newSemantic

    refine st oldSemantic newSemantic | oldSemantic == newSemantic =
        findProgram env dst net st
    refine st oldSemantic newSemantic = do
        net <- withTime "petri net init" (initNet env)
        st' <- withTime "petri net encoding" (resetEncoder env dst net)
        findProgram env dst net st'

    checkSolution st codes = do
        solutions <- view currentSolutions <$> get
        checkedSols <- filterM (liftIO . haskellTypeChecks env dst) codes
        if (null checkedSols) || (checkedSols `union` solutions == solutions)
           then do
               findProgram env dst net st
           else do
               let solution = head checkedSols
               printSolution solution
               modify $ over currentSolutions ((:) solution)
               return $ (solution, st)

    printSolution solution = do
        liftIO $ putStrLn "*******************SOLUTION*********************"
        liftIO $ print solution
        liftIO $ putStrLn "************************************************"

findFirstN :: (MonadIO m) => Environment -> RType -> PetriNet -> EncodeState -> Int -> PNSolver m RProgram
findFirstN env dst net st cnt | cnt == 1  = do
    (res, _) <- findProgram env dst net st
    return res
findFirstN env dst net st cnt | otherwise = do
    (_, st') <- findProgram env dst net st
    findFirstN env dst net st' (cnt-1)

-------------------------------------------------------------------------------
-- | helper functions
-------------------------------------------------------------------------------

writeLog level msg = do
    st <- get
    if level <= st ^. logLevel then traceShow (plain msg) $ return () else return ()

multiPermutation len elmts | len == 0 = []
multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
multiPermutation len elmts            = nubOrd $ [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]

fillAny arg = AExclusion Set.empty
