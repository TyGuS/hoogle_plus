module Examples.InferenceDriver
  ( InfStats(..)
  , parseExample
  , checkExamples
  , resolveType
  , getExampleTypes
  , generalizeType
  , generalizeTypeSkeleton

    -- * type inference query
  , searchTypes
  ) where

import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Lens                   ( (^.)
                                                , makeLenses
                                                , over
                                                , set
                                                , view
                                                )
import           Control.Monad.Logic            ( LogicT
                                                , MonadLogic(ifte)
                                                , observeAllT
                                                )
import           Control.Monad.State
import           Data.Bifunctor                 ( second )
import qualified Data.ByteString.Lazy.Char8    as LB
import           Data.Char                      ( chr
                                                , isLower
                                                , ord
                                                )
import           Data.Either                    ( isRight
                                                , partitionEithers
                                                )
import           Data.List                      ( (\\)
                                                , delete
                                                , intercalate
                                                , isInfixOf
                                                , partition
                                                , sort
                                                , sortOn
                                                )
import           Data.List.Extra                ( groupOn
                                                , nubOrdBy
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , isJust
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           GHC                     hiding ( Id )
import           Outputable                     ( Outputable(ppr)
                                                , showSDocUnsafe
                                                )
import           Text.Printf                    ( printf )

import Database.Dataset
import Examples.Utils
import HooglePlus.IOFormat
import Types.Antiunifier
import Types.Common
import Types.Environment
import Types.Fresh
import Types.Pretty
import Types.Program
import Types.Substitution
import Types.Type
import Types.TypeChecker
import Types.TypeClass
import Utility.GHCUtils
import Utility.Utils

data InfStats = InfStats {
  _prefilterCounts  :: Int,
  _postfilterCounts :: Int
} deriving (Eq, Show)

makeLenses ''InfStats

data GeneralizationState = GeneralizationState {
  _typeCounter   :: Map TypeSkeleton (Id, Int),
  _nameCounter   :: Map Id Int,
  _prevTypeVars  :: Set Id,
  _beginTypeVars :: Set Id
} deriving Eq

makeLenses ''GeneralizationState

emptyGenState :: GeneralizationState
emptyGenState = GeneralizationState Map.empty Map.empty Set.empty Set.empty

type StatRecorder m = StateT InfStats (MonadCounter (MonadTycl m))
type TypeGeneralizer m = StateT GeneralizationState (LogicT (StatRecorder m))
data AntiUnifResult = AntiUnifResult TypeSkeleton AntiunifState

runStatRecorder :: Monad m => StatRecorder m a -> m (a, InfStats)
runStatRecorder go = runMonadTycl $ runMonadCounter $ runStateT go (InfStats (-1) (-1))

--------------------------------------------------------------------------------
-------------------------------- Example Checker -------------------------------
--------------------------------------------------------------------------------

checkExample :: [Id] -> Environment -> SchemaSkeleton -> Example -> IO (Either GHCError SchemaSkeleton)
checkExample mdls env typ ex = do
  eitherTyp <- parseExample mdls mkFun
  case eitherTyp of
    Right exTyp -> do
      let err = printf "%s does not have type %s" (plainShow ex) (plainShow typ) :: String
      let tcErr = printf "%s does not satisfy type class constraint in %s" (show ex) (plainShow typ) :: String
      -- refresh the type variables names in the two
      let bvs = getBoundTypeVars env
      (freshExTyp, freshTyp) <- evalStateT
        (do t1 <- toMonotype <$> freshSchema bvs exTyp
            t2 <- toMonotype <$> freshSchema bvs typ
            return (t1, t2))
        (Map.empty :: Map Id Int)
      let mbTass = solveTypeConstraint [] Map.empty (UnifiesWith freshExTyp freshTyp)
      case mbTass of
        Nothing   -> return $ Left err
        Just tass -> do
          let substedTyp = apply tass freshTyp
          let (tyclasses, strippedTyp) = unprefixTc substedTyp
          let tyclassesPrenex = intercalate ", " $ map plainShow tyclasses
          let breakTypes = map plainShow $ breakdown strippedTyp
          let mkTyclass = printf "%s :: (%s) => (%s)" mkFun tyclassesPrenex (intercalate ", " breakTypes)
          eitherTyclass <- parseExample mdls mkTyclass
          if null tyclasses || isRight eitherTyclass
            then return $ Right exTyp
            else return $ Left tcErr
    Left e -> return $ Left e
 where
  mkFun = printf "(%s)" (intercalate ", " $ inputs ex ++ [output ex])

  unprefixTc (FunctionT x tArg tRes) = case tArg of
    DatatypeT name args | tyclassPrefix `Text.isPrefixOf` name ->
      let (tcs, t) = unprefixTc tRes
          currTc   = DatatypeT (Text.drop (Text.length tyclassPrefix) name) args
      in  (currTc : tcs, t)
    _ -> ([], FunctionT x tArg tRes)
  unprefixTc t = ([], t)

checkExamples :: [Id] -> Environment -> SchemaSkeleton -> [Example] -> IO (Either [GHCError] [SchemaSkeleton])
checkExamples mdls env typ exs = do
  outExs <- mapM (checkExample mdls env typ) exs
  let (errs, validResults) = partitionEithers outExs
  if null errs then return $ Right validResults else return $ Left errs

--------------------------------------------------------------------------------
-------------------------------- Type Inference --------------------------------
--------------------------------------------------------------------------------

searchTypes :: String -> Int -> IO (ListOutput String, InfStats)
searchTypes inStr num = do
  let input   = decodeInput (LB.pack inStr)
  let exquery = inExamples input
  let mkFun ex = printf "(%s)" (intercalate ", " $ inputs ex ++ [output ex])
  exTypes <- mapM (parseExample includedModules . mkFun) exquery
  let (invalidTypes, validSchemas) = partitionEithers exTypes
  let argNames = inArgNames input
  resultObj <- if null invalidTypes
    then possibleQueries argNames exquery validSchemas
    else return (ListOutput [] (unlines invalidTypes), InfStats (-1) (-1))
  printResult $ encodeWithPrefix $ fst resultObj
  return resultObj
 where
  renameVars t =
    let fvars  = Set.toList $ freeVars t
        validVars = foldr delete seqChars fvars
        substVars = foldr delete fvars seqChars
        substMap  = Map.fromList $ zip substVars $ map vart validVars
    in  apply substMap t

  possibleQueries argNames exquery exTypes = do
    (generalTypes, stats) <- getExampleTypes argNames exTypes num
    if null generalTypes
      then return
        (ListOutput [] "Cannot find type for your query", InfStats 0 0)
      else return (ListOutput generalTypes "", stats)

-- pre: the mkFun is a tuple of arguments and return types
parseExample :: [Id] -> String -> IO (Either GHCError SchemaSkeleton)
parseExample mdls mkFun = catch
  (do
    typ <- askGhc mdls $ exprType TM_Default mkFun
    let hsType = typeToLHsType typ
    let hpType = toInt $ resolveType hsType
    return (Right hpType))
  (\(e :: SomeException) -> return (Left $ show e))
 where
  toInt (ForallT x t) = ForallT x (toInt t)
  toInt (Monotype t ) = Monotype (integerToInt t)

getExampleTypes :: [Id] -> [SchemaSkeleton] -> Int -> IO ([String], InfStats)
getExampleTypes argNames validSchemas num = do
  (typs, stat) <- runStatRecorder (lift (observeAllT go) >>= mapM (\(AntiUnifResult t st) -> zip (repeat t) <$> getGeneralizations t st))
  let sortedTyps = sortOn (\(t, (tc, gt)) -> scoreSignature t tc gt) (concat typs)
  let tcTyps = map (addTyclasses . second (addArgNames argNames) . snd) sortedTyps
  return (take num tcTyps, stat)
 where
  stepAntiUnification :: MonadIO m => AntiUnifResult -> TypeSkeleton -> LogicT (MonadCounter (MonadTycl m)) AntiUnifResult
  stepAntiUnification (AntiUnifResult t1 st) t2 = uncurry AntiUnifResult <$> runStateT (antiunify t1 t2) st

  go :: MonadIO m => LogicT (MonadCounter (MonadTycl m)) AntiUnifResult
  go = do freshTypes <- lift $ mapM (freshSchema []) validSchemas
          let validTypes = map toMonotype freshTypes
          foldM stepAntiUnification (AntiUnifResult (head validTypes) emptyAntiunifState) (tail validTypes)

  getGeneralizations :: MonadIO m => TypeSkeleton -> AntiunifState -> StatRecorder m [(TyclAssignment, TypeSkeleton)]
  getGeneralizations t st = do
    let tcass = st ^. tyclAssignment
    if isInhabited tcass t
      then generalizeType validSchemas tcass t
      else return []

  toConstraint (id, s) =
    Text.intercalate ", " $ map (Text.unwords . (: [id])) (Set.toList s)

  addTyclasses (constraints, t) =
    let
      tyclasses = Text.intercalate ", " $ filter (not . Text.null) $ map toConstraint renamedConstraints
      vars = Set.toList (freeVars t)
      letters = map Text.singleton ['a' .. 'z']
      varCandidates = filter (`notElem` vars) letters
      varSubst = Map.fromList (filter (uncurry (/=)) $ zip vars varCandidates)
      renamedTyp = apply (Map.map vart varSubst) t
      substInConstraint = \(id, s) -> (Map.findWithDefault id id varSubst, s)
      renamedConstraints = map substInConstraint (Map.toList constraints)
      strTyp = show $ plain $ prettyTypeWithName renamedTyp
    in if Text.null tyclasses then strTyp else printf "(%s) => %s" tyclasses strTyp

  addArgNames [] t = t
  addArgNames (arg : args) (FunctionT _ tArg tRes) =
    FunctionT arg tArg (addArgNames args tRes)
  addArgNames _ _ =
    error "unmatched number of arguments between argNames and query type"

  padding t xs = zip (repeat (fst t)) xs
  flattenResult (antiUnifs, generals) = zipWith padding antiUnifs generals

antiSubstitute :: TypeSkeleton -> Id -> TypeSkeleton -> TypeSkeleton
antiSubstitute pat name t | t == pat = vart name
antiSubstitute pat name (DatatypeT dt args) =
  DatatypeT dt (map (antiSubstitute pat name) args)
antiSubstitute pat name (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
 where
  tArg' = antiSubstitute pat name tArg
  tRes' = antiSubstitute pat name tRes
antiSubstitute _ _ t = t

generalizeType :: MonadIO m => [SchemaSkeleton] -> TyclAssignment -> TypeSkeleton -> StatRecorder m [(TyclAssignment, TypeSkeleton)]
generalizeType exTyps tcass t = nubOrdBy dedupArgs <$> observeAllT go
  where
    go = do
      let (_, vars) = Set.partition isExistential (variablesOf t)
      let vars' = Set.unions (Set.map freeVars vars)
      (tyclAss, gt) <- evalStateT (generalizeTypeSkeleton tcass t) (emptyGenState { _beginTypeVars = vars' })
      let (fvars, vars) = Set.partition isExistential (variablesOf gt)
      let fvars' = Set.map (Set.findMin . freeVars) fvars
      if Set.null vars && Set.null fvars
        then modify (over prefilterCounts (+ 1))
          >> modify (over postfilterCounts (+ 1))
          >> return (tyclAss, gt)
        else msum $ map (getInhabited fvars' tyclAss gt) (Set.toList vars)

    getInhabited :: MonadIO m => Set Id -> TyclAssignment -> TypeSkeleton -> TypeSkeleton -> LogicT (StateT InfStats m) (TyclAssignment, TypeSkeleton)
    getInhabited fvars tyclAss gt (TypeVarT _ v)  = do
      let t = apply (Map.fromSet (const $ vart v) fvars) gt
      modify $ over prefilterCounts (+ 1)
      guard (isInhabited tyclAss t)
      modify $ over postfilterCounts (+ 1)
      return (tyclAss, t)
    getInhabited _ _ _ t = error $ "unexpected type in getInhabited: " ++ plainShow t

generalizeTypeSkeleton :: MonadIO m => TyclAssignment -> TypeSkeleton -> TypeGeneralizer m (TyclAssignment, TypeSkeleton)
generalizeTypeSkeleton tcass t@(TypeVarT _ id) = do
  let tc = maybe [] Set.toList (Map.lookup id tcass)
  let mkResult tc = return (Map.singleton id (Set.singleton tc), t)
  prevVars <- gets $ view prevTypeVars
  if id `Set.member` prevVars
    then return (Map.empty, t)
    else do modify $ over prevTypeVars (Set.insert id)
            msum (map mkResult tc) `mplus` return (Map.empty, t)
generalizeTypeSkeleton tcass t@(FunctionT x tArg tRes) = do
  (argTyclass, tArg') <- generalizeTypeSkeleton tcass tArg
  (resTyclass, tRes') <- generalizeTypeSkeleton tcass tRes
  let tyclassAssignment = Map.foldrWithKey (Map.insertWith Set.union) resTyclass argTyclass
  return (tyclassAssignment, FunctionT x tArg' tRes')
generalizeTypeSkeleton tcass t@(DatatypeT name args)
  | null args || (plainShow t == "[Char]") = datatypeToVar tcass t
  | otherwise = do
    generalizedArgs <- mapM (generalizeTypeSkeleton tcass) args
    let (tyclassMaps, args') = unzip generalizedArgs
    let argTyclasses = foldr (Map.foldrWithKey (Map.insertWith Set.union)) Map.empty tyclassMaps
    return (argTyclasses, DatatypeT name args')
generalizeTypeSkeleton _ t = error $ "unsupported type " ++ plainShow t

datatypeToVar :: MonadIO m => TyclAssignment -> TypeSkeleton -> TypeGeneralizer m (TyclAssignment, TypeSkeleton)
datatypeToVar tcass t = do
  -- generalize the data type into a fresh type variable
  -- or reuse previous variables if tyclass checks
  tccache      <- lift . lift . lift . lift $ get
  typeCounting <- gets $ view typeCounter
  prevVars     <- gets $ view prevTypeVars
  beginVars    <- gets $ view beginTypeVars
  dtclasses    <- case Map.lookup t tccache of
    Just tc -> return tc
    Nothing -> lift . lift . lift . lift $ Set.toList <$> getDtTycls tcass t
  -- TODO: what is the good option to post-filter this cases
  -- because it is not always correct
  let matchedPrevVars = filter
        (\v -> case Map.lookup v tcass of
          Just tc -> not (Set.null (tc `Set.intersection` Set.fromList dtclasses))
          Nothing -> False)
        (Set.toList beginVars)
  let (v, startIdx) = case Map.lookup t typeCounting of
        Just (v, i) -> (v, i)
        Nothing ->
          let vars = Set.toList prevVars ++ map fst (Map.elems typeCounting)
              currMaxOrd = maximum (ord 'a' : map (ord . Text.head) vars)
          in  (Text.pack [chr (currMaxOrd + 1)], 0)
  -- choose between incr the index or not
  -- if reusing some previous var, do not backtrack over type classes
  msum (map (\v -> do modify $ over prevTypeVars (Set.insert v)
                      return (Map.empty, vart v)
            ) matchedPrevVars)
    `mplus` msum (map (\idx -> return (Map.empty, vart (appendIndex v idx))) [1 .. startIdx])
    `mplus`
      -- if start a new index
      if startIdx < 3 -- optimization for eval only
        then do
          let varName = appendIndex v (startIdx + 1)
          modify $ over typeCounter $ Map.insert t (v, startIdx + 1)
          modify $ over nameCounter $ Map.insert v (startIdx + 1)
          return (Map.empty, vart varName) `mplus` msum
            (map (\tc -> return (Map.singleton varName (Set.singleton tc), vart varName)) dtclasses)
        else mzero

defaultTypeVar :: TypeSkeleton -> TypeSkeleton -> TypeSkeleton
defaultTypeVar to t@(TypeVarT Exists v) = to
defaultTypeVar to (DatatypeT name args) =
  DatatypeT name (map (defaultTypeVar to) args)
defaultTypeVar to (FunctionT x tArg tRes) =
  let tArg' = defaultTypeVar to tArg
      tRes' = defaultTypeVar to tRes
  in  FunctionT x tArg' tRes'
defaultTypeVar _ t = t

-- partition all the type variables in the given type
-- into negative set and positive set
reversePolarity :: Bool -> TypeSkeleton -> Bool
reversePolarity reversed t =
  if isFunctionType t then not reversed else reversed

typeVarPolarity :: Bool -> TypeSkeleton -> (Set Id, Set Id)
typeVarPolarity reversed (TypeVarT _ v) = if reversed
  then (Set.empty, Set.singleton v)
  else (Set.singleton v, Set.empty)
typeVarPolarity reversed (DatatypeT _ args) =
  let argPolarities =
        map (\t -> typeVarPolarity (reversePolarity reversed t) t) args
      (neg, pos) = unzip argPolarities
  in  (Set.unions neg, Set.unions pos)
typeVarPolarity reversed (FunctionT x tArg tRes) =
  let (neg , pos ) = typeVarPolarity (reversePolarity reversed tArg) tArg
      (neg', pos') = typeVarPolarity (not $ reversePolarity reversed tRes) tRes
  in  (neg `Set.union` neg', pos `Set.union` pos')
typeVarPolarity _ _ = error "typeVarPolarity: not a valid type"

notOnlyPositive :: TypeSkeleton -> Bool
notOnlyPositive t =
  let (neg, pos) = typeVarPolarity False t
  in  Set.null (pos `Set.difference` neg)

isInhabited :: TyclAssignment -> TypeSkeleton -> Bool
isInhabited tyclass t =
  let tcArgs = concatMap tyclassesToArgs $ Map.toList tyclass
      args   = map snd (argsWithName t)
      res    = lastType t
  in  isReachable args res && all (isRelevant (tcArgs ++ args) res) args
 where
  tyclassToArg v tc
    | tc == "Eq" || tc == "Ord" = FunctionT "" (vart v) (FunctionT "" (vart v) boolType)
    | tc == "Num" = FunctionT ""
                              (vart v)
                              (FunctionT "" (vart v) (vart v))
    | otherwise = error "Unsupported type class"

  tyclassesToArgs (v, tcs) =
    map (uncurry tyclassToArg . (v, )) $ Set.toList tcs

isReachable :: [TypeSkeleton] -> TypeSkeleton -> Bool
isReachable args res =
  freeVars res `Set.isSubsetOf` Set.unions (map freeVars args)

isRelevant :: [TypeSkeleton] -> TypeSkeleton -> TypeSkeleton -> Bool
isRelevant args res (TypeVarT _ id) =
  (id `Set.member` freeVars res) || usedInHigherOrder
 where
  hoArgs            = filter isFunctionType args ++ concatMap hoArgsOf args
  usedInHigherOrder = any relevantToHigherOrder hoArgs
  relevantToHigherOrder hoArg =
    let argsOfHoArg = map snd (argsWithName hoArg)
        tvInArgs    = Set.unions $ map freeVars argsOfHoArg
    in  (id `Set.member` tvInArgs) && isRelevant args res hoArg
isRelevant args res t@FunctionT{} =
  let argsOfHoArg   = map snd (argsWithName t)
      resOfHoArg    = lastType t
      containedArgs = containsType t args
      remainingArgs = args \\ containedArgs
      argsReachable = all (isReachable remainingArgs) argsOfHoArg
      resRelevant   = isRelevant (resOfHoArg : remainingArgs) res resOfHoArg
  in  argsReachable && resRelevant
isRelevant _ _ _ = True

checkImplies :: [Id] -> Bool
checkImplies tcs = "Ord" `notElem` tcs || "Eq" `notElem` tcs

dedupArgs :: (TyclAssignment, TypeSkeleton) -> (TyclAssignment, TypeSkeleton) -> Ordering
dedupArgs (tc1, t1) (tc2, t2) =
  let args1 = map snd (argsWithName t1)
      args2 = map snd (argsWithName t2)
      ret1  = lastType t1
      ret2  = lastType t2
  in  compare (sort args1, ret1, tc1) (sort args2, ret2, tc2)

reverseSubstitution :: TypeSkeleton -> TypeSkeleton -> Map TypeSkeleton (Set TypeSkeleton) -> Map TypeSkeleton (Set TypeSkeleton)
reverseSubstitution t1@TypeVarT{} t2 tass = Map.insertWith Set.union t1 (Set.singleton t2) tass
reverseSubstitution t1 t2@TypeVarT{} tass = Map.insertWith Set.union t1 (Set.singleton t2) tass
reverseSubstitution t1@(DatatypeT name1 args1) t2@(DatatypeT name2 args2) tass
  | null args1 && null args2 = Map.insertWith Set.union
                                              t1
                                              (Set.singleton t2)
                                              tass
  | name1 == name2 = foldr (uncurry reverseSubstitution) tass (zip args1 args2)
  | name1 /= name2 = error "datatype was lost during generalization"
reverseSubstitution (FunctionT _ tArg1 tRes1) (FunctionT _ tArg2 tRes2) tass =
  let tass' = reverseSubstitution tArg1 tArg2 tass
  in  reverseSubstitution tRes1 tRes2 tass'
reverseSubstitution _ _ _ = error "reverseSubstitution: unexpected type"

scoreSignature :: TypeSkeleton -> TyclAssignment -> TypeSkeleton -> Double
scoreSignature auType tyclass t =
  let subst     = reverseSubstitution auType t Map.empty
      tcCount   = 0.5 * fromIntegral (Map.foldr ((+) . Set.size) 0 tyclass)
      varsCount = Set.size $ freeVars t
      substCount k s = Set.size s
  in  fromIntegral (Map.foldrWithKey (\k v -> (+) $ substCount k v) 0 subst)
        + -- penalty for more than one non-identity subst
          tcCount
        - 0.1
        * fromIntegral varsCount