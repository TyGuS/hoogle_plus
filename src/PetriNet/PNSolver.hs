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
  | Combination
  | QueryRefinement
  deriving(Data, Show, Eq)

data TimeStatistics = TimeStatistics {
  encodingTime :: Double,
  constructionTime :: Double,
  solverTime :: Double,
  codeFormerTime :: Double,
  refineTime :: Double,
  typeCheckerTime :: Double,
  otherTime :: Double,
  totalTime :: Double,
  iterations :: Int,
  numOfTransitions :: Map Int Int,
  numOfPlaces :: Map Int Int
} deriving(Eq)

data SolverState = SolverState {
    _nameCounter :: Map Id Int,  -- name map for generating fresh names (type variables, parameters)
    _typeAssignment :: Map Id SType,  -- current type assignment for each type variable
    _typingError :: (SType, SType), -- typing error message, represented by the expected type and actual type
    _abstractionTree :: AbstractionTree,
    _isChecked :: Bool, -- is the current state check passed
    _currentSolutions :: [RProgram], -- type checked solutions
    _currentLoc :: Int, -- current solution depth
    _currentSigs :: Map Id AbstractSkeleton, -- current type signature groups
    _detailedSigs :: Set Id,
    _functionMap :: HashMap Id FunctionCode,
    _targetType :: Id,
    _sourceTypes :: [Id],
    _paramNames :: [Id],
    _refineStrategy :: RefineStrategy,
    _groupMap :: Map Id [Id], -- mapping from group id to list of function names
    _type2transition :: Map AbstractSkeleton [Id], -- mapping from abstract type to group ids
    _solverNet :: PetriNet,
    _solverStats :: TimeStatistics,
    _useGroup :: Bool,
    _logLevel :: Int, -- temporary for log level
    _maxApplicationDepth :: Int
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
    _detailedSigs = Set.empty,
    _functionMap = HashMap.empty,
    _targetType = "",
    _sourceTypes = [],
    _paramNames = [],
    _refineStrategy = NoRefine,
    _groupMap = Map.empty,
    _type2transition = Map.empty,
    _solverNet = PetriNet HashMap.empty HashMap.empty HashMap.empty,
    _solverStats = TimeStatistics (0::Double) (0::Double) (0::Double) (0) (0::Double) (0::Double) (0::Double) (0::Double) 0 Map.empty Map.empty,
    _useGroup = False,
    _logLevel = 0,
    _maxApplicationDepth = 6
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
encodeFunction id t@(AFunctionT tArg tRet) = FunctionCode id hoParams params [show $ lastAbstractType t]
  where
    base = (0, [])
    hoFun x (i, a) = (i+1, (encodeFunction ("f" ++ show i ++ "_" ++ id) x) : a)
    paramFun x (i, a) = if isAFunctionT x then (i+1, ("f" ++ show i ++ "_" ++ id):a) else (i, (show x):a)
    (_, hoParams) = foldr hoFun base $ filter isAFunctionT (abstractParamList t)
    (_, params) = foldr paramFun base (abstractParamList t)
encodeFunction id t = FunctionCode id [] [] [show t]

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

instantiate :: (MonadIO m) => Environment -> [(Id, AbstractSkeleton)] -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs = do
    semantic <- view abstractionTree <$> get
    Map.fromList <$> instantiate' sigs (nonPolyAbstracts semantic)
  where
    nonPolySigs = filter (not . hasAbstractVar . snd) sigs
    nonPolyAbstracts semantic = map (\(id, t) -> (id, head (cutoff env semantic t))) nonPolySigs
    removeSuffix id ty = (removeLast '_' id, ty)

    instantiate' sigs sigsAcc = do
        st <- get
        -- to test first level abstraction, please disable the complement type here
        let typs = case st ^. refineStrategy of
                     AbstractRefinement -> leafTypes (st ^. abstractionTree)
                     NoRefine -> filter notEx (leafTypes (st ^. abstractionTree))
                     Combination -> filter notEx (leafTypes (st ^. abstractionTree))
                     QueryRefinement -> (leafTypes (st ^. abstractionTree))
        writeLog 3 $ text "Current abstract types:" <+> pretty typs
        sigs' <- foldM (\acc -> (<$>) ((++) acc) . uncurry (instantiateWith env typs)) [] sigs
        return $ nubOrdOn (uncurry removeSuffix) (sigsAcc ++ sigs')

instantiateWith :: (MonadIO m) => Environment -> [AbstractSkeleton] -> Id -> AbstractSkeleton -> PNSolver m [(Id, AbstractSkeleton)]
instantiateWith env typs id sk = do
    let vars = Set.toList $ allAbstractVar sk
    let multiSubsts = map (zip vars) $ multiPermutation (length vars) typs
    let substedSymbols = map (foldr (\(id, t) acc -> abstractSubstitute (env ^. boundTypeVars) id t acc) sk) multiSubsts
    st <- get
    let refinedSymbols = nubOrd (concatMap (cutoff env (st ^. abstractionTree)) substedSymbols)
    foldrM (\t accMap -> do
        newId <- newSymbolName id
        return $ (newId, t):accMap) [] refinedSymbols
    where

        newSymbolName prefix = do
            indices <- flip (^.) nameCounter <$> get
            let idx = Map.findWithDefault 0 prefix indices
            modify (over nameCounter $ Map.insert prefix (idx+1))
            return $ prefix ++ "_" ++ show idx

splitTransition :: MonadIO m => Environment -> Id -> SplitInfo -> PNSolver m SplitInfo
splitTransition env tid info = do
    writeLog 3 $ pretty tid
    sigs <- view currentSigs <$> get
    let splitedTyp = oldPlace info
    let newTyps = newPlace info
    let typ = transitionSig sigs
    -- step 1: unify the type with refined abstract type and get new signatures
    unifyRes <- unifyNewType typ newTyps
    let unifiedTyps = nubOrdOn snd unifyRes
    -- step 2: add new transitions into the environment
    mapM_ (\(id, ty) -> modify $ over currentSigs (Map.insert id ty)) unifiedTyps
    mapM_ (\(id, ty) -> modify $ over functionMap (HashMap.insert id (encodeFunction id ty))) unifiedTyps
    mapM_ (uncurry typeMap) unifiedTyps
    -- step 3: pack the information into SplitInfo for incremental encoding
    let newIds = fst (unzip unifiedTyps)
    -- step 4: remove splited transition from the type2transition mapping
    modify $ over type2transition (Map.map (delete tid))
    -- return the new split information with splited transitions
    -- step 5: update the detailed signature ids
    modify $ over detailedSigs (Set.union (Set.fromList newIds) . Set.delete tid)
    if null newIds
       then return info
       else if "Pair" `isPrefixOf` tid
                then do
                    -- step 6: add pair pattern matching when we are splitting some Pair constructors, they share the same number with constructor
                    let tid' = Text.unpack (Text.replace "Pair" "Pair_match" (Text.pack tid))
                    let pairs = map (\(id, ty) -> mkPairMatch (encodeFunction id ty)) unifiedTyps
                    mapM_ (\ef -> modify $ over functionMap (HashMap.insert (funName ef) ef)) pairs
                    let newIds' = map funName pairs
                    return (info { splitedGroup = (tid', newIds'):(tid, newIds):(splitedGroup info) })
                else return (info { splitedGroup = (tid, newIds):(splitedGroup info) })
  where

    getHoParams (AFunctionT tArg tRet) = init (decompose tArg) ++ getHoParams tRet
    getHoParams _ = []

    typeMap id ty = do
        let tys = decompose ty
        mapM_ (\t -> modify $ over type2transition (Map.insertWith union t [id])) tys

    transitionSig sigs = case Map.lookup tid sigs of
                                Just sig -> sig
                                Nothing -> error $ printf "cannot find transition %s in sig map" tid

    matchTyps target (AFunctionT tArg tRet) (AFunctionT tArg' tRet') =
        matchTyps target tArg tArg' ++ matchTyps target tRet tRet'
    matchTyps target absTyp polyTyp
        | target == absTyp = [polyTyp]
        | otherwise = []

    typeConstraints (AFunctionT tArg tRet) (AFunctionT tArg' tRet') =
        typeConstraints tArg tArg' ++ typeConstraints tRet tRet'
    typeConstraints t1 t2 = [(t1, t2)]

    genTypes pattern newTyps (AFunctionT tArg tRet) =
            [ AFunctionT arg ret | arg <- if null args then [tArg] else args
                                 , ret <- if null rets then [tRet] else rets ]
        where
            args = genTypes pattern newTyps tArg
            rets = genTypes pattern newTyps tRet
    genTypes pattern newTyps t = if t == pattern then newTyps else []

    checkUnification tree bound tass t1 t2 | t1 == t2 = Just tass
    checkUnification tree bound tass (ATypeVarT id) (ATypeVarT id') | id `elem` bound && id' `elem` bound = Nothing
    checkUnification tree bound tass (ATypeVarT id) (ATypeVarT id') | not (id `elem` bound) && not (id' `elem` bound) = Nothing
    checkUnification tree bound tass (ATypeVarT id) t | id `elem` bound = checkUnification tree bound tass t (ATypeVarT id)
    checkUnification tree bound tass (ATypeVarT id) t | id `Map.member` tass =
            if isJust commonAssigned
               then Just (Map.insert id (fromJust commonAssigned) tass)
               else Nothing
        where
            assigned = fromJust (Map.lookup id tass)
            commonAssigned = abstractIntersection t assigned
    checkUnification tree bound tass (ADatatypeT {}) (ATypeVarT id) | id `elem` bound = Nothing
    checkUnification tree bound tass (AExclusion s) (ATypeVarT id) | id `elem` bound && id `Set.member` s = Nothing
    checkUnification tree bound tass (AExclusion s) (ATypeVarT id) | id `elem` bound && id `Set.notMember` s = Just tass
    checkUnification tree bound tass t (ATypeVarT id) | not (id `elem` bound) = checkUnification tree bound tass (ATypeVarT id) t
    checkUnification tree bound tass (ATypeVarT id) t = Just (Map.insert id t tass)
    checkUnification tree bound tass (ADatatypeT id tArgs) (ADatatypeT id' tArgs') | id /= id' = Nothing
    checkUnification tree bound tass (ADatatypeT id tArgs) (ADatatypeT id' tArgs') | id == id' = checkArgs tass tArgs tArgs'
        where
            checkArgs m [] [] = Just m
            checkArgs m (arg:args) (arg':args') =
                case checkUnification tree bound m arg arg' of
                    Nothing -> Nothing
                    Just m'  -> checkArgs m' args args'
    checkUnification _ bound tass (ADatatypeT id tArgs) (AExclusion s) | id `Set.member` s = Nothing
    checkUnification _ bound tass (AExclusion s) (ADatatypeT id tArgs) | id `Set.member` s = Nothing
    checkUnification _ bound tass (ADatatypeT {}) (AExclusion {}) = Just tass
    checkUnification _ bound tass (AExclusion {}) (ADatatypeT {}) = Just tass
    checkUnification _ bound tass _ _ = Nothing

    getUnifier _ _ Nothing _ = Nothing
    getUnifier tree bound tass [] = tass
    getUnifier tree bound (Just tass) ((t1, t2):cs) =
        case checkUnification tree bound tass t1 t2 of
            Nothing -> Nothing
            Just m  -> getUnifier tree bound (Just m) cs

    unifyNewType typ newTyps = do
        let id' = removeLast '_' tid
        polyTyp <- toAbstractType . shape <$> findSymbol env id'
        -- pass the current abstract hierarchy into the unifier
        abstraction <- view abstractionTree <$> get
        -- get all the constraints
        let typs = genTypes (oldPlace info) newTyps typ
        let constraints = map (\t -> typeConstraints t polyTyp) typs
        writeLog 3 $ text "trying to solve constraints" <+> pretty constraints
        let unifiers = map (getUnifier abstraction (env ^. boundTypeVars) (Just Map.empty)) constraints
        writeLog 3 $ text "unify result is" <+> pretty (show unifiers)
        let checkSuccess = filter isJust unifiers
        if not (null checkSuccess)
           then do
               let ts = snd (unzip (filter (isJust . fst) (zip unifiers typs)))
               writeLog 3 $ text "get signatures" <+> pretty ts
               ids <- mapM (\_ -> freshId id') ts
               return (zip ids ts)
           else return []

cutoff :: Environment -> AbstractionTree -> AbstractSkeleton -> [AbstractSkeleton]
cutoff env semantic (ATypeVarT id) | not (isBound env id) = [rightmostType semantic]
cutoff env semantic typ@(ATypeVarT id) | isBound env id =
    case semantic of
      ALeaf t -> [t]
      ANode t lt rt | t == typ -> [typ]
      ANode t lt rt | isSubtypeOf typ (valueType lt) -> cutoff env lt typ
      ANode t lt rt | isSubtypeOf typ (valueType rt) -> cutoff env rt typ
cutoff _ (ALeaf t) (ADatatypeT id tArgs) = [t]
cutoff env (ANode _ lt rt) typ@(ADatatypeT {}) | isSubtypeOf typ (valueType lt) = cutoff env lt typ
cutoff env (ANode _ lt rt) typ@(ADatatypeT {}) | isSubtypeOf typ (valueType rt) = cutoff env rt typ
cutoff env semantic@(ANode t lt rt) typ@(ADatatypeT {}) | isSubtypeOf t typ = leafTypes semantic
cutoff env semantic (AFunctionT tArg tRet) =
    [ AFunctionT a r | a <- args, r <- rets, isJust (isConsistent (AFunctionT tArg tRet) (AFunctionT a r)) ]
  where
    args = cutoff env semantic tArg
    rets = cutoff env semantic tRet

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
cutoff _ semantic t@(AExclusion _) = [rightmostType semantic]
cutoff _ semantic t = filter (isJust . abstractIntersection t) (leafTypes (closestTree semantic t))

updateSemantic :: Environment -> AbstractionTree -> AbstractSkeleton -> AbstractionTree
updateSemantic env semantic typ@(ATypeVarT id) | isBound env id =
  case semantic of
    ALeaf t
        | t == typ -> semantic
        | otherwise -> ANode t (ALeaf typ) (ALeaf (typeDifference t typ))
    ANode t lt rt
        | t == typ -> semantic
        | isSubtypeOf typ t && isSubtypeOf typ (valueType lt) -> ANode t (updateSemantic env lt typ) rt
        | isSubtypeOf typ t -> ANode t lt (updateSemantic env rt typ)
        | otherwise -> error (printf "%s is not a subtype of %s, not a subtype of %s, not a subtype of %s, we should not reach here" (show typ) (show t) (show (valueType lt)) (show (valueType rt)))
updateSemantic _ semantic (ATypeVarT id) | otherwise = semantic
updateSemantic _ semantic@(ALeaf t) typ@(ADatatypeT {}) | typ == t = semantic
updateSemantic env semantic@(ALeaf t@(ADatatypeT id tArgs)) typ@(ADatatypeT id' tArgs') =
  case (tArgs, tArgs') of
    ([], []) -> semantic
    _ -> firstDiff semantic [] tArgs tArgs'
  where
    wrapTree preArgs postArgs (ALeaf t) = ALeaf (ADatatypeT id (preArgs ++ [t] ++ postArgs))
    wrapTree preArgs postArgs (ANode t lt rt) =
        ANode (ADatatypeT id (preArgs ++ [t] ++ postArgs))
              (wrapTree preArgs postArgs lt)
              (wrapTree preArgs postArgs rt)

    rootType (ALeaf t) = t
    rootType (ANode t _ _) = t

    replaceTree (ALeaf t) tree | rootType tree == t = tree
    replaceTree (ANode t lt rt) tree | rootType tree == t = error "should not replace a non-leaf type"
    replaceTree (ANode t lt rt) tree | isSubtypeOf (rootType tree) (valueType lt) = ANode t (replaceTree lt tree) rt
    replaceTree (ANode t lt rt) tree | isSubtypeOf (rootType tree) (valueType rt) = ANode t lt (replaceTree rt tree)

    firstDiff s _ [] [] = s
    firstDiff s pre (arg:args) (arg':args')
      | arg == arg' = firstDiff s (pre ++ [arg]) args args'
      | otherwise   = let tmp = updateSemantic env (ALeaf arg) arg'
                          s' = replaceTree s (wrapTree pre args tmp)
                      in firstDiff s' (pre ++ [arg']) args args'
updateSemantic env semantic@(ALeaf t) typ@(ADatatypeT id tArgs) | t /= typ =
    updateSemantic env semantic' typ
  where
    emptyArgs = map fillAny tArgs
    typ' = ADatatypeT id emptyArgs
    semantic' = ANode t (ALeaf typ') (ALeaf (typeDifference t typ'))
updateSemantic env semantic@(ANode t lt rt) typ@(ADatatypeT id tArgs)
  | t == typ = semantic
  | isSubtypeOf typ t && isSubtypeOf typ (valueType lt) = ANode t (updateSemantic env lt typ) rt
  | isSubtypeOf typ t = ANode t lt (updateSemantic env rt typ)
  | otherwise = error (printf "%s is not a subtype of %s, not a subtype of %s, not a subtype of %s, we should not reach here" (show typ) (show t) (show (valueType lt)) (show (valueType rt)))
updateSemantic env semantic (AFunctionT tArg tRet) = semantic''
  where
    semantic' = updateSemantic env semantic tArg
    semantic'' = updateSemantic env semantic' tRet
updateSemantic _ semantic (AExclusion _) = semantic

-- distinguish one type from a given general one
type SplitMsg = (AbstractSkeleton, AbstractSkeleton)

distinguish :: MonadIO m => Environment -> SType -> SType -> PNSolver m (Maybe SplitMsg)
distinguish env (FunctionT _ tArg tRes) (FunctionT _ tArg' tRes') = do
    diff <- distinguish env tArg tArg'
    case diff of
      Nothing  -> distinguish env tRes tRes'
      res -> return res
distinguish env AnyT _ = return Nothing
distinguish env _ AnyT = return Nothing
distinguish env t1 t2 = do
    tass <- view typeAssignment <$> get
    semantic <- view abstractionTree <$> get
    writeLog 2 $ text "type assignments" <+> text (show tass)
    let t1' = var2any (stypeSubstitute tass t1)
    let t2' = var2any (stypeSubstitute tass t2)
    let ats1 = cutoff env semantic (toAbstractType t1')
    let ats2 = cutoff env semantic (toAbstractType t2')
    writeLog 3 $ text "trying to distinguish" <+> pretty t1' <+> text "==>" <+> pretty ats1 <+> text "and" <+> pretty t2' <+> text "==>" <+> pretty ats2
    -- only try to get split information when the two types have
    -- same abstract representations in the current abstraction level
    let diff = ats1 `intersect` ats2
    let pTyp = head diff
    let aTyp1 = toAbstractType t1'
    let aTyp2 = toAbstractType t2'
    if null diff || t1' == t2'
       then return Nothing
       else case distinguish' (env ^. boundTypeVars) pTyp aTyp1 aTyp2 of
              Nothing -> return Nothing
              Just t -> return (Just (pTyp, t))
  where
    var2any t@(ScalarT (TypeVarT _ id) _) | isBound env id = t
    var2any t@(ScalarT (TypeVarT _ id) _) | otherwise = AnyT
    var2any (ScalarT (DatatypeT id args l) r) = ScalarT (DatatypeT id (map var2any args) l) r
    var2any (FunctionT x tArg tRet) = FunctionT x (var2any tArg) (var2any tRet)

distinguishFrom :: AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
distinguishFrom (AExclusion s) t@(ATypeVarT id) | id `Set.notMember` s = t
distinguishFrom (AExclusion s) t@(ADatatypeT id _) | id `Set.notMember` s = t
distinguishFrom (ADatatypeT id tArgs) (ADatatypeT id' tArgs') | id == id' =
    ADatatypeT id (firstDiff tArgs tArgs')
  where
    firstDiff [] [] = []
    firstDiff (arg:args) (arg':args')
        | arg == arg' = arg:(firstDiff args args')
        | otherwise = (distinguishFrom arg arg'):args
distinguishFrom t1 t2 = error ("Cannot distinguish " ++ show t2 ++ " from " ++ show t1)

distinguish' :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton -> Maybe AbstractSkeleton
distinguish' _ _ t1 t2 | t1 == t2 = Nothing
distinguish' _ _ t1@(ADatatypeT id1 tArgs1) t2@(ADatatypeT id2 tArgs2) | id1 /= id2 =
    Just (ADatatypeT id1 (map fillAny tArgs1))
distinguish' _ (AExclusion {}) t1@(ADatatypeT id1 tArgs1) t2@(ADatatypeT id2 tArgs2) | id1 == id2 =
    Just (ADatatypeT id1 (map fillAny tArgs1))
distinguish' tvs (ADatatypeT pid pArgs) t1@(ADatatypeT id1 tArgs1) t2@(ADatatypeT id2 tArgs2) | id1 == id2 =
    case firstDifference pArgs tArgs1 tArgs2 of
      [] -> Nothing
      diffs -> Just (ADatatypeT id1 diffs)
  where
    firstDifference _ [] [] = []
    firstDifference (parg:pargs) (arg:args) (arg':args') =
        case distinguish' tvs parg arg arg' of
            Nothing -> case firstDifference pargs args args' of
                         [] -> []
                         diffs -> parg:diffs
            Just t  -> if t /= parg then t:pargs
                                    else case firstDifference pargs args args' of
                                           [] -> []
                                           diffs -> parg : diffs
distinguish' _ _ (AExclusion s) (ADatatypeT id args) | id `Set.notMember` s = Just (ADatatypeT id (map fillAny args))
distinguish' _ _ (AExclusion s) (ADatatypeT id _) = Nothing
distinguish' tvs p t1@(ADatatypeT id args) t2@(AExclusion s) = distinguish' tvs p t2 t1
distinguish' _ _ (ATypeVarT id1) (ADatatypeT id2 args) = Just (ATypeVarT id1) -- Just (ADatatypeT id2 (map fillAny args))
distinguish' tvs p t1@(ADatatypeT {}) t2@(ATypeVarT {}) = distinguish' tvs p t2 t1
distinguish' tvs _ (AExclusion s) (ATypeVarT id) | id `elem` tvs && id `Set.notMember` s = Just (ATypeVarT id)
distinguish' _ _ (AExclusion {}) (ATypeVarT id) = Nothing
distinguish' tvs p t1@(ATypeVarT {}) t2@(AExclusion {}) = distinguish' tvs p t2 t1
distinguish' tvs _ (ATypeVarT id) (ATypeVarT _) | id `elem` tvs = Just (ATypeVarT id)
distinguish' tvs _ (ATypeVarT _) (ATypeVarT id) | id `elem` tvs = Just (ATypeVarT id)
distinguish' tvs _ (ATypeVarT _) (ATypeVarT _) = Nothing
distinguish' _ _ t1 t2 = error (printf "unhandled case for distinguish %s and %s" (show t1) (show t2))

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

bottomUpCheck :: (MonadIO m) => Environment -> RProgram -> PNSolver m RProgram
bottomUpCheck env p@(Program PHole _) = return p
bottomUpCheck env p@(Program (PSymbol sym) typ) = do
    -- lookup the symbol type in current scope
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    t <- findSymbol env sym
    return (Program (PSymbol sym) t)
bottomUpCheck env (Program (PApp pFun pArg) typ) = do
    fun <- bottomUpCheck env pFun
    let FunctionT _ tArg tRet = typeOf fun
    arg <- bottomUpCheck env pArg
    writeLog 3 $ text "Solving constraint for" <+> pretty arg <+> text "::" <+> pretty (shape $ typeOf arg) <+> text "==" <+> pretty (shape tArg)
    solveTypeConstraint env (shape $ typeOf arg) (shape tArg)
    return (Program (PApp fun arg) tRet)
bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    body' <- bottomUpCheck (addVariable x (addTrue tArg) env) body
    return (Program (PFun x body') (FunctionT x tArg (typeOf body')))
bottomUpCheck env p@(Program (PFun x body) _) = do
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    id <- freshId "A"
    let tArg = addTrue (ScalarT (TypeVarT Map.empty id) ())
    body' <- bottomUpCheck (addVariable x tArg env) body
    return (Program (PFun x body') (FunctionT x tArg (typeOf body')))
bottomUpCheck _ p = error ("unhandled case for checking " ++ show p ++ "::" ++ show (typeOf p))

solveTypeConstraint :: (MonadIO m) => Environment -> SType -> SType -> PNSolver m ()
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
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
solveTypeConstraint env t1 t2 = error $ "unknown types " ++ show t1 ++ " or " ++ show t2

-- | unify the type variable with some given type
-- add the type assignment to our state
unify :: (MonadIO m) => Environment -> Id -> SType -> PNSolver m ()
unify env v t = do
    modify $ over typeAssignment (Map.map (stypeSubstitute (Map.singleton v t)))
    tass <- view typeAssignment <$> get
    modify $ over typeAssignment (Map.insert v (stypeSubstitute tass t))

-- | compare the programs tagged with types from bidirectional type checking
-- try to distinguish the first pair of different concrete types with the same
-- abstract representation
compProg :: MonadIO m => Environment -> RProgram -> RProgram -> PNSolver m (Maybe SplitMsg)
compProg env p1@(Program (PSymbol {}) t1) p2@(Program (PSymbol {}) t2) = do
    writeLog 3 $ text "comparing" <+> pretty p1 <+> text "::" <+> pretty t1 <+> text "and" <+> pretty p2 <+> text "::" <+> pretty t2
    distinguish env (shape t1) (shape t2)
compProg env p1@(Program PHole t1) p2@(Program _ t2) = do
    writeLog 3 $ text "comparing" <+> pretty p1 <+> text "::" <+> pretty t1 <+> text "and" <+> pretty p2 <+> text "::" <+> pretty t2
    distinguish env (shape t1) (shape t2)
compProg env p1@(Program _ t1) p2@(Program PHole t2) = do
    writeLog 3 $ text "comparing" <+> pretty p1 <+> text "::" <+> pretty t1 <+> text "and" <+> pretty p2 <+> text "::" <+> pretty t2
    distinguish env (shape t1) (shape t2)
compProg env p1@(Program (PApp pFun1 pArg1) t1) p2@(Program (PApp pFun2 pArg2) t2) = do
    writeLog 3 $ text "comparing" <+> pretty p1 <+> text "::" <+> pretty t1 <+> text "and" <+> pretty p2 <+> text "::" <+> pretty t2
    compArg <- compProg env pArg1 pArg2
    case compArg of
      Nothing -> do
        compFun <- compProg env pFun1 pFun2
        case compFun of
          Nothing -> distinguish env (shape t1) (shape t2)
          Just _  -> return compFun
      Just _  -> return compArg
compProg env (Program (PFun x1 body1) t1) (Program (PFun x2 body2) t2) = compProg env body1 body2

-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: MonadIO m => Environment -> UProgram -> RProgram -> PNSolver m SplitInfo
refineSemantic env prog top = do
    modify $ set isChecked True
    btm <- bottomUpCheck env prog
    writeLog 3 $ text "bottom up checking get program" <+> pretty btm
    checkRes <- compProg env top btm
    case checkRes of
        Nothing -> do
            writeLog 2 $ text "two programs do not give us new split message"
            return (SplitInfo (AExclusion Set.empty) [] [])
        Just (t, ts) -> do
            semantic <- view abstractionTree <$> get
            let t':_ = cutoff env semantic t
            let semantic' = updateSemantic env semantic ts
            writeLog 2 $ text $ printf "before update semantic, %s is splited into %s" (show t') (show ts)
            writeLog 2 $ text $ printf "new semantics is %s" (show semantic')
            modify $ set abstractionTree semantic'
            let ts' = head (cutoff env semantic' ts)
            writeLog 3 $ text $ printf "%s is splited into %s" (show t') (show ts')
            t2tr <- view type2transition <$> get
            let tids = Map.findWithDefault [] t' t2tr
            let splitNode = SplitInfo t' ((leafTypes semantic') \\ (leafTypes semantic)) []
            foldrM (splitTransition env) splitNode tids

-- | wrap some action with time measuring and print out the execution time
withTime :: MonadIO m => String -> PNSolver m a -> PNSolver m a
withTime desc f = do
    start <- liftIO getCPUTime
    res <- f
    end <- liftIO getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    let time = printf "%s: %0.3f sec\n" desc (diff :: Double)
    modify $ over solverStats (\s ->
        case desc of
          "construction time" -> s { constructionTime = (constructionTime s) + (diff :: Double) }
          "encoding time" -> s { encodingTime = (encodingTime s) + (diff :: Double) }
          "code former time" -> s { codeFormerTime = (codeFormerTime s) + (diff :: Double) }
          "solver time" -> s { solverTime = (solverTime s) + (diff :: Double) }
          "refinement time" -> s { refineTime = (refineTime s) + (diff :: Double) }
          "type checking time" -> s { typeCheckerTime = (typeCheckerTime s) + (diff :: Double) }
          "total search time" -> s {totalTime = (diff :: Double) }
          _ -> s { otherTime = (otherTime s) + (diff :: Double) })
    writeLog 1 $ text time
    return res

resetTiming :: Monad m => PNSolver m ()
resetTiming = do
  modify $ over solverStats (\s ->
    s { encodingTime=0,
        codeFormerTime=0,
        solverTime=0,
        refineTime=0,
        typeCheckerTime=0,
        totalTime=0
      })

initNet :: MonadIO m => Environment -> PNSolver m ()
initNet env = withTime "construction time" $ do
    -- reset the solver state
    modify $ set functionMap HashMap.empty
    modify $ set currentSigs Map.empty
    modify $ set type2transition Map.empty

    let binds = env ^. boundTypeVars
    abstraction <- view abstractionTree <$> get
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    let srcTypes = map ( head
                       . cutoff env abstraction
                       . toAbstractType
                       . shape
                       . toMonotype) $ Map.elems foArgs
    modify $ set sourceTypes (map show srcTypes)
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    absSymbols <- mapM (uncurry abstractSymbol) (Map.toList (allSymbols env))
    -- first order arguments are tokens but not transitions in petri net
    let usefulSymbols = filter (flip notElem ("fst" : "snd" : Map.keys foArgs) . fst) absSymbols
    sigs <- instantiate env usefulSymbols
    modify $ set detailedSigs (Map.keysSet sigs)
    writeLog 3 $ text "instantiated sigs" <+> pretty (Map.toList sigs)
    symbols <- mapM addEncodedFunction (Map.toList sigs)
    let symbols' = concat symbols
    modify $ set solverNet (buildPetriNet symbols' (map show srcTypes))
  where
    abstractSymbol id sch = do
        t <- freshType sch
        let absTy = toAbstractType (shape t) -- abstract binds abstraction (shape t)
        return (id, absTy)

    addEncodedFunction (id, f) | "Pair" `isPrefixOf` id = do
        let ef = encodeFunction id f
        modify $ over functionMap (HashMap.insert (replaceId "Pair" "Pair_match" id) (mkPairMatch ef))
        modify $ over functionMap (HashMap.insert id ef)
        modify $ over currentSigs (Map.insert id f)
        -- store the used abstract types and their groups into mapping
        let addTransition k tid = Map.insertWith union k [tid]
        let includedTyps = decompose f
        mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps
        return [ef, mkPairMatch ef]
    addEncodedFunction (id, f) = do
        let ef = encodeFunction id f
        modify $ over functionMap (HashMap.insert id ef)
        modify $ over currentSigs (Map.insert id f)
        -- store the used abstract types and their groups into mapping
        let addTransition k tid = Map.insertWith union k [tid]
        let includedTyps = decompose f
        mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps

        return [ef]

cloneArg :: Id -> FunctionCode
cloneArg id = FunctionCode (id ++ "|clone") [] [id] [id, id]

decomposeHoSym :: FunctionCode -> [FunctionCode]
decomposeHoSym (FunctionCode _ [] _ _) = []
decomposeHoSym (FunctionCode name hoParams params ret) =
    let foParam = filter (not . isPrefixOf "f") params
        nestedHoSym = concatMap decomposeHoSym hoParams
        unfoldHoSym = concatMap (uncurry (decomposeHoParam name foParam ret)) (zip hoParams [1,2..])
    in nestedHoSym ++ unfoldHoSym

decomposeHoParam name foParams outRets (FunctionCode _ _ params ret) i =
      (createEntry i name foParams params)
    : (createExit i name ret outRets)
    : (map (createDiscard i name) params)
  where
    createEntry i name params rets =
        let entry = name ++ "|entry" ++ (show i)
            special = name ++ "|special" ++ (show i)
         in FunctionCode entry [] params (special:rets)

    createExit i name params rets =
        let exit = name ++ "|exit" ++ (show i)
            special = name ++ "|special" ++ (show i)
        in FunctionCode exit [] (special:params) rets

    -- strategy for discarding higher order arguments parameters
    -- the main purpose of the design here is to give this operation a high cost
    -- the cost can be measured by giving the transition a large weight
    -- or the operation can be interpreted into more than one transition
    createDiscard i name param =
        let discard = name ++ "|discard" ++ (show i) ++ "_" ++ (show param)
            special = name ++ "|special" ++ (show i)
        in FunctionCode discard [] [param, special] ["void", special]

resetEncoder :: (MonadIO m) => Environment -> RType -> PNSolver m EncodeState
resetEncoder env dst = do
    -- reset source and destination types
    let binds = env ^. boundTypeVars
    abstraction <- view abstractionTree <$> get
    let tgt = show (head (cutoff env abstraction (toAbstractType (shape dst))))
    modify $ set targetType tgt
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    let srcTypes = map ( show
                       . head
                       . cutoff env abstraction
                       . toAbstractType
                       . shape
                       . toMonotype) $ Map.elems foArgs
    modify $ set sourceTypes srcTypes
    modify $ set paramNames $ Map.keys foArgs
    srcTypes <- view sourceTypes <$> get
    writeLog 2 $ text "parameter types are" <+> pretty srcTypes
    writeLog 2 $ text "return type is" <+> pretty tgt

    -- reset the petri net
    net <- view solverNet <$> get
    modify $ over solverStats (\s -> s {
        iterations = iterations s + 1,
        numOfTransitions = Map.insert (iterations s + 1) (HashMap.size (pnTransitions net)) (numOfTransitions s),
        numOfPlaces = Map.insert (iterations s + 1) (HashMap.size (pnPlaces net)) (numOfPlaces s)
    })
    -- filter the net with only the detailed signatures
    dsigs <- view detailedSigs <$> get
    sigs <- view currentSigs <$> get
    let removedIds = Map.keysSet sigs `Set.difference` dsigs
    writeLog 3 $ text "removing transitions" <+> (text (show removedIds))
    let net' = Set.foldr removeTransition net removedIds
    modify $ set solverNet net'
    loc <- view currentLoc <$> get
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    liftIO $ encoderInit net' loc hoArgs srcTypes tgt

findPath :: (MonadIO m) => Environment -> RType -> EncodeState -> PNSolver m (CodePieces, EncodeState)
findPath env dst st = do
    (res, st') <- withTime "solver time" (liftIO (encoderSolve st))
    case res of
        [] -> do
            currSt <- get
            maxDepth <- view maxApplicationDepth <$> get
            when (currSt ^. currentLoc >= maxDepth) (error "cannot find a path")
            modify $ set currentLoc ((currSt ^. currentLoc) + 1)
            -- initNet env
            st'' <- withTime "encoding time" (resetEncoder env dst)
            findPath env dst st''
        _  -> withTime "code former time" $ do
            fm <- view functionMap <$> get
            src <- view sourceTypes <$> get
            args <- view paramNames <$> get
            let sortedRes = sortOn snd res
            let transNames = map fst sortedRes
            writeLog 2 $ text "found path" <+> pretty transNames
            let usefulTrans = filter (\n -> skipEntry n
                                         && skipClone n
                                         && skipDiscard n) transNames
            let sigNames = map removeSuffix usefulTrans
            dsigs <- view detailedSigs <$> get
            let sigNames' = filter (\name -> Set.member name dsigs || "Pair_match" `isPrefixOf` name) sigNames
            let sigs = substPair (map (findFunction fm) sigNames')
            writeLog 2 $ text "found filtered sigs" <+> text (show sigs)
            -- let sigs = combinations (map (findFunctions fm groups) sigNames)
            let initialFormer = FormerState 0 HashMap.empty [] []
            code <- generateCode initialFormer src args sigs
            return (code, st')
  where
    findFunctions fm groups name =
        case Map.lookup name groups of
            Just g -> findFunction fm (head g) -- map (findFunction fm) (head g)
            Nothing -> error $ "cannot find function group " ++ name

    findFunction fm name =
        case HashMap.lookup name fm of
            Just fc -> fc
            Nothing -> error $ "cannot find function name " ++ name

    combinations []    = []
    combinations [x]   = [x]
    combinations (h:t) = [ hh:tt | hh <- h, tt <- combinations t ]

    generateCode initialFormer src args sigs = do
        tgt <- view targetType <$> get
        liftIO (evalStateT (generateProgram sigs src args tgt True) initialFormer)
    skipEntry = not . isInfixOf "|entry"
    skipClone = not . isInfixOf "|clone"
    skipDiscard = not . isInfixOf "|discard"
    removeSuffix = removeLast '|'

    substPair [] = []
    substPair (x:xs) = if "Pair_match" `isPrefixOf` (funName x)
                          then   ( x { funName = "fst", funReturn = [(funReturn x) !! 0] } )
                               : ( x { funName = "snd", funReturn = [(funReturn x) !! 1] } )
                               : (substPair xs)
                          else x:(substPair xs)

fixNet :: MonadIO m => Environment -> SplitInfo -> PNSolver m ()
fixNet env (SplitInfo _ nts splitedGps) = do
    -- reset the src types with the new abstraction semantic
    abstraction <- view abstractionTree <$> get
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    let srcTypes = map ( show
                       . head
                       . cutoff env abstraction
                       . toAbstractType
                       . shape
                       . toMonotype) $ Map.elems foArgs
    oldSource <- view sourceTypes <$> get
    modify $ set sourceTypes srcTypes
    -- add new function into the petri net
    let newGroups = concat (snd (unzip splitedGps))
    sigs <- view currentSigs <$> get
    net <- view solverNet <$> get
    let sigs' = Map.filterWithKey (\k _ -> k `elem` newGroups) sigs
    writeLog 3 $ text "sigs to be added include" <+> text (show sigs')
    fm <- view functionMap <$> get
    let encodedFuncs = map (\name -> fromJust (HashMap.lookup name fm)) newGroups
    let functionedNet = foldr addArgClone (foldr addFunction net encodedFuncs) (map show nts)
    modify $ set solverNet functionedNet
        {- if oldSource == srcTypes
       then modify $ set solverNet functionedNet
       else do
           let justTypes = map (\(t1, t2)-> if t1 == t2 then Nothing else Just t2) (zip oldSource srcTypes)
           let diffTypes = map fromJust (filter isJust justTypes)
            -- add input argument clone transitions
           let argClones = map cloneArg (nubOrd diffTypes)
           modify $ set solverNet (foldr addFunction functionedNet argClones)
        -}
fixEncoder :: MonadIO m => Environment -> RType -> EncodeState -> SplitInfo -> PNSolver m EncodeState
fixEncoder env dst st info = do
    let binds = env ^. boundTypeVars
    abstraction <- view abstractionTree <$> get
    let tgt = show (head (cutoff env abstraction (toAbstractType (shape dst))))
    modify $ set targetType tgt
    srcTypes <- view sourceTypes <$> get
    writeLog 2 $ text "fixed parameter types are" <+> pretty srcTypes
    writeLog 2 $ text "fixed return type is" <+> pretty tgt
    loc <- view currentLoc <$> get
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    writeLog 2 $ text ("get split information " ++ show info)
    net <- view solverNet <$> get
    -- writeLog 2 $ text ("new fixed net " ++ show net)
    liftIO $ execStateT (encoderRefine net info srcTypes tgt) st

findProgram :: MonadIO m => Environment -> RType -> EncodeState -> PNSolver m (RProgram, EncodeState)
findProgram env dst st = do
    modify $ set typeAssignment Map.empty
    writeLog 2 $ text "calling findProgram"
    (codeResult, st') <- findPath env dst st
    oldSemantic <- view abstractionTree <$> get
    writeLog 2 $ pretty (Set.toList codeResult)
    checkResult <- withTime "type checking time" (mapM parseAndCheck $ Set.toList codeResult)
    let codes = lefts checkResult
    let errors = rights checkResult
    if null codes && null errors
       then do
           -- we didn't find a solution for it
           let st'' = st' { prevChecked = True }
           findProgram env dst st''
       else if null codes
               then findNextSolution st' oldSemantic (head errors)
               else checkSolution st' codes
  where
    parseAndCheck code = do
        let prog = case parseExp code of
                       ParseOk exp -> toSynquidProgram exp
                       ParseFailed loc err -> error err
        writeLog 1 $ text "Find program" <+> pretty prog
        modify $ set isChecked True
        top <- topDownCheck env (shape dst) prog
        writeLog 3 $ text "top down checking get program" <+> pretty top
        ifM (view isChecked <$> get)
            (return (Left top))
            (return (Right [prog, top]))

    findNextSolution st oldSemantic errs@[errorProg, errorTop] = do
        rs <- view refineStrategy <$> get
        case rs of
            NoRefine -> findProgram env dst st
            Combination -> do
              splitInfo <- withTime "refinement time" (refineSemantic env errorProg errorTop)
              -- add new places and transitions into the petri net
              newSemantic <- view abstractionTree <$> get
              refine st oldSemantic newSemantic splitInfo
            AbstractRefinement -> do
              splitInfo <- withTime "refinement time" (refineSemantic env errorProg errorTop)
              -- add new places and transitions into the petri net
              newSemantic <- view abstractionTree <$> get
              refine st oldSemantic newSemantic splitInfo
            QueryRefinement -> do
              splitInfo <- withTime "refinement time" (refineSemantic env errorProg errorTop)
              -- add new places and transitions into the petri net
              newSemantic <- view abstractionTree <$> get
              refine st oldSemantic newSemantic splitInfo

    refine st oldSemantic newSemantic info | oldSemantic == newSemantic = do
        let st' = st { prevChecked = True }
        findProgram env dst st'
    refine st oldSemantic newSemantic info = do
        withTime "construction time" (fixNet env info)
        net' <- view solverNet <$> get
        modify $ over solverStats (\s -> s {
            iterations = iterations s + 1,
            numOfTransitions = Map.insert (iterations s + 1) (HashMap.size (pnTransitions net')) (numOfTransitions s),
            numOfPlaces = Map.insert (iterations s + 1) (HashMap.size (pnPlaces net')) (numOfPlaces s)
        })
        -- net' <- withTime "petri net init" (initNet env)
        st' <- withTime "encoding time" (fixEncoder env dst st info)
        sigs <- view currentSigs <$> get
        dsigs <- view detailedSigs <$> get
        -- liftIO $ print (Map.filterWithKey (\k _ -> Set.member k dsigs) sigs)
        -- st' <- withTime "petri net encoding" (resetEncoder env dst net')
        findProgram env dst st'

    checkSolution st codes = do
        solutions <- view currentSolutions <$> get
        -- let checkedSols = codes
        checkedSols <- withTime "type checking time" (filterM (liftIO . haskellTypeChecks env dst) codes)
        if (null checkedSols) || (solutions `union` checkedSols == solutions)
           then do
               findProgram env dst st
           else do
               let solution = head checkedSols
               -- printSolution solution
               -- printStats
               modify $ over currentSolutions ((:) solution)
               let st' = st { prevChecked = True }
               return $ (solution, st')

printSolution solution = do
    liftIO $ putStrLn "*******************SOLUTION*********************"
    liftIO $ putStrLn $ "SOLUTION: " ++ (mkOneLine $ show solution)
    liftIO $ putStrLn "************************************************"

printStats :: MonadIO m => PNSolver m ()
printStats = do
    stats <- view solverStats <$> get
    depth <- view currentLoc <$> get
    liftIO $ putStrLn "*******************STATISTICS*******************"
    liftIO $ putStrLn ("Search time for solution: " ++ (showFullPrecision (totalTime stats)))
    liftIO $ putStrLn ("Petri net construction time: " ++ (showFullPrecision (constructionTime stats)))
    liftIO $ putStrLn ("Petri net encoding time: " ++ (showFullPrecision (encodingTime stats)))
    liftIO $ putStrLn ("Z3 solving time: " ++ (showFullPrecision (solverTime stats)))
    liftIO $ putStrLn ("Hoogle plus code former time: " ++ (showFullPrecision (codeFormerTime stats)))
    liftIO $ putStrLn ("Hoogle plus refinement time: " ++ (showFullPrecision (refineTime stats)))
    liftIO $ putStrLn ("Hoogle plus type checking time: " ++ (showFullPrecision (typeCheckerTime stats)))
    liftIO $ putStrLn ("Total iterations of refinements: " ++ (show (iterations stats)))
    liftIO $ putStrLn ("Number of places: " ++ (show $ map snd (Map.toAscList (numOfPlaces stats))))
    liftIO $ putStrLn ("Number of transitions: " ++ (show $ map snd (Map.toAscList (numOfTransitions stats))))
    liftIO $ putStrLn ("Solution Depth: " ++ show depth)
    liftIO $ putStrLn "********************END STATISTICS****************************"

findFirstN :: (MonadIO m) => Environment -> RType -> EncodeState -> Int -> PNSolver m RProgram
findFirstN env dst st cnt | cnt == 1  = do
    (res, _) <- withTime "total search time" $ findProgram env dst st
    printSolution res
    printStats
    return res
findFirstN env dst st cnt | otherwise = do
    (res, st') <- withTime "total search time" $ findProgram env dst st
    printSolution res
    printStats
    resetTiming
    findFirstN env dst st' (cnt-1)

runPNSolver :: MonadIO m => Environment -> Int -> RType -> PNSolver m RProgram
runPNSolver env cnt t = do
    initNet env
    st <- withTime "encoding time" (resetEncoder env t)
    findFirstN env t st cnt


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

replaceId a b = Text.unpack . Text.replace a b . Text.pack
mkPairMatch (FunctionCode name _ params ret) = FunctionCode (replaceId "Pair" "Pair_match" name) [] ret params
