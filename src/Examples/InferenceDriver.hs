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

import           Database.Dataset
import           Examples.Utils
import           HooglePlus.IOFormat
import           Types.Common
import           Types.Environment
import           Types.Fresh
import           Types.Pretty
import           Types.Program
import           Types.Type
import qualified Types.TypeChecker
import Types.Substitution
import           Utility.Utils

type TyclassConstraints = Set Id
type TyclassAssignment = Map Id TyclassConstraints

data InfStats = InfStats
  { _prefilterCounts  :: Int
  , _postfilterCounts :: Int
  }
  deriving (Eq, Show)

makeLenses ''InfStats

data TypeClassState = TypeClassState
  { _tyclassCache   :: Map TypeSkeleton [Id]
  , _supportModules :: [Id]
  , _generalNames   :: Map Id Int
  ,
    -- stats
    _infStats       :: InfStats
  }
  deriving Eq

makeLenses ''TypeClassState

emptyTyclassState :: TypeClassState
emptyTyclassState = TypeClassState { _tyclassCache   = Map.empty
                                   , _supportModules = []
                                   , _generalNames   = Map.empty
                                   , _infStats       = InfStats 0 0
                                   }

type AntiPair = (TypeSkeleton, TypeSkeleton)
data AntiUnifConstraint =
    UnifConstraint TypeSkeleton TypeSkeleton
  | DisunifConstraint TypeSkeleton TypeSkeleton
  deriving(Eq)

data AntiUnifState = AntiUnifState
  { _typeAssignment     :: Map AntiPair Id
  , _tyclassAssignment  :: TyclassAssignment
  ,
    -- temporary constraints to be satisfied during anti-unification
    _unifConstraints    :: [AntiUnifConstraint]
  , _disunifConstraints :: [AntiUnifConstraint]
  , _tmpAssignment      :: Map Id TypeSkeleton
  }
  deriving Eq

makeLenses ''AntiUnifState

instance Monad m => Fresh TypeClassState m where
  nextCounter prefix = do
    names <- gets $ view generalNames
    let idx = Map.findWithDefault 0 prefix names
    modify $ over generalNames (Map.insert prefix (idx + 1))
    return idx

emptyAntiUnifState :: AntiUnifState
emptyAntiUnifState = AntiUnifState { _typeAssignment     = Map.empty
                                   , _tyclassAssignment  = Map.empty
                                   , _unifConstraints    = []
                                   , _disunifConstraints = []
                                   , _tmpAssignment      = Map.empty
                                   }

data TypeNaming = TypeNaming
  { _substCounter  :: Map TypeSkeleton (Id, Int)
  , _nameCounter   :: Map Id Int
  , _prevTypeVars  :: Set Id
  , _beginTypeVars :: Set Id
  }
  deriving Eq

makeLenses ''TypeNaming

type AntiUnifier m = StateT AntiUnifState (LogicT (StateT TypeClassState m))
type TypeGeneralizer m = StateT TypeNaming (LogicT (StateT TypeClassState m))
type TypeInferer m = LogicT (StateT TypeClassState m)

data AntiUnifResult = AntiUnifResult TypeSkeleton AntiUnifState

supportedTyclasses :: [Id]
supportedTyclasses = ["Num", "Ord", "Eq"]

--------------------------------------------------------------------------------
-------------------------------- Example Checker -------------------------------
--------------------------------------------------------------------------------

checkExample
  :: [Id]
  -> Environment
  -> SchemaSkeleton
  -> Example
  -> IO (Either GHCError SchemaSkeleton)
checkExample mdls env typ ex = do
  eitherTyp <- parseExample mdls mkFun
  case eitherTyp of
    Right exTyp -> do
      let err =
            printf "%s does not have type %s" (plainShow ex) (plainShow typ) :: String
      let tcErr =
            printf "%s does not satisfy type class constraint in %s"
                   (show ex)
                   (plainShow typ) :: String
      -- refresh the type variables names in the two
      let bvs = getBoundTypeVars env
      (freshExTyp, freshTyp) <- evalStateT
        (do
          t1 <- toMonotype <$> freshSchema bvs exTyp
          t2 <- toMonotype <$> freshSchema bvs typ
          return (t1, t2)
        )
        (Map.empty :: Map Id Int)
      let mbTass = checkTypes env [] freshExTyp freshTyp
      case mbTass of
        Nothing   -> return $ Left err
        Just tass -> do
          let substedTyp               = apply tass freshTyp
          let (tyclasses, strippedTyp) = unprefixTc substedTyp
          let tyclassesPrenex          = intercalate ", " $ map plainShow tyclasses
          let breakTypes               = map plainShow $ breakdown strippedTyp
          let mkTyclass = printf "%s :: (%s) => (%s)"
                                 mkFun
                                 tyclassesPrenex
                                 (intercalate ", " breakTypes)
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

checkExamples
  :: [Id]
  -> Environment
  -> SchemaSkeleton
  -> [Example]
  -> IO (Either [GHCError] [SchemaSkeleton])
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
  let argNames                     = inArgNames input
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
        substMap  = Map.fromList $ zip substVars $ map TypeVarT validVars
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
    return (Right hpType)
  )
  (\(e :: SomeException) -> return (Left $ show e))
 where
  toInt (ForallT x t) = ForallT x (toInt t)
  toInt (Monotype t ) = Monotype (integerToInt t)

getExampleTypes :: [Id] -> [SchemaSkeleton] -> Int -> IO ([String], InfStats)
getExampleTypes argNames validSchemas num = do
  let initTyclassState = emptyTyclassState
  -- remove magic number later
  (typs, st) <- runStateT
    (do
      mbFreeVars <- observeAllT go
      mapM
        (\(AntiUnifResult t st) -> zip (repeat t) <$> getGeneralizations t st)
        mbFreeVars
    )
    initTyclassState
  let sortedTyps =
        sortOn (\(t, (tc, gt)) -> scoreSignature t tc gt) (concat typs)
  let tcTyps =
        map (addTyclasses . second (addArgNames argNames) . snd) sortedTyps
  return (take num tcTyps, st ^. infStats)
 where
  stepAntiUnification
    :: MonadIO m
    => AntiUnifResult
    -> TypeSkeleton
    -> TypeInferer m AntiUnifResult
  stepAntiUnification (AntiUnifResult t1 st) t2 = antiUnification t1 t2 st

  go :: MonadIO m => TypeInferer m AntiUnifResult
  go = do
      -- liftIO $ print validSchemas
      -- create types with all fresh type variables
    freshTypes <- lift $ mapM (freshSchema []) validSchemas
    let validTypes = map toMonotype freshTypes
    foldM stepAntiUnification
          (AntiUnifResult (head validTypes) emptyAntiUnifState)
          (tail validTypes)

  forall t = foldr ForallT (Monotype t) (freeVars t)

  getGeneralizations t st = do
    let tvars = freeVars t
    let tcass = st ^. tyclassAssignment
    -- either contains wildcards or is inhabited
    if hasWildcard t || isInhabited tcass t
      then generalizeType validSchemas tcass t
      else return []

  toConstraint (id, s) =
    Text.intercalate ", " $ map (Text.unwords . (: [id])) (Set.toList s)

  addTyclasses (constraints, t) =
    let
      tyclasses = Text.intercalate ", " $ filter (not . Text.null) $ map
        toConstraint
        renamedConstraints
      vars               = Set.toList (freeVars t)
      letters            = map Text.singleton ['a' .. 'z']
      varCandidates      = filter (`notElem` vars) letters
      varSubst = Map.fromList (filter (uncurry (/=)) $ zip vars varCandidates)
      renamedTyp         = apply (Map.map TypeVarT varSubst) t
      substInConstraint  = \(id, s) -> (Map.findWithDefault id id varSubst, s)
      renamedConstraints = map substInConstraint (Map.toList constraints)
      strTyp             = show $ plain $ prettyTypeWithName renamedTyp
    in
      if Text.null tyclasses
        then strTyp
        else printf "(%s) => %s" tyclasses strTyp

  addArgNames [] t = t
  addArgNames (arg : args) (FunctionT _ tArg tRes) =
    FunctionT arg tArg (addArgNames args tRes)
  addArgNames _ _ =
    error "unmatched number of arguments between argNames and query type"

  padding t xs = zip (repeat (fst t)) xs
  flattenResult (antiUnifs, generals) = zipWith padding antiUnifs generals

-- turn a GHC type into Hoogle+ type
resolveType :: LHsType GhcPs -> SchemaSkeleton
resolveType (L _ (HsForAllTy _ _ bs t)) = foldr ForallT (resolveType t) vs
 where
  vs = map (appendSuffix wildcardPrefix . vname) bs

  vname :: LHsTyVarBndr GhcPs -> String
  vname (L _ (UserTyVar _ (L _ id))) = showSDocUnsafe (ppr id)
  vname (L _ (KindedTyVar _ (L _ id) _)) = showSDocUnsafe (ppr id)
  vname _ = error "not implemented"
-- TODO (zhg): why do we need this case?
-- resolveType t@(L _ (HsAppTy _ fun arg)) =
--   let typs          = tail $ map resolveType' $ breakApp t
--       (args, [res]) = splitAt (length typs - 1) typs
--   in  Monotype $ foldr (FunctionT "") res args
--  where
--   breakApp (L _ (HsAppTy _ fun arg)) = breakApp fun ++ [arg]
--   breakApp t                         = [t]
resolveType (L _ (HsQualTy _ ctx body)) = Monotype bodyWithTcArgs
 where
  unlocatedCtx  = let L _ c = ctx in c
  tyConstraints = map (prefixTyclass . resolveType') unlocatedCtx

  prefixTyclass (DatatypeT name args) =
    DatatypeT (tyclassPrefix `Text.append` name) args
  prefixTyclass tc = error $ "Unsupported type class " ++ show tc

  bodyWithTcArgs =
    foldr (FunctionT "") (toMonotype $ resolveType body) tyConstraints
resolveType t = Monotype $ resolveType' t

resolveType' :: LHsType GhcPs -> TypeSkeleton
resolveType' (L _ (HsFunTy _ f r)) =
  FunctionT "" (resolveType' f) (resolveType' r)
resolveType' (L _ (HsQualTy _ _ t      )) = resolveType' t
resolveType' (L _ (HsTyVar  _ _ (L _ v))) = if isLower (head name)
  then TypeVarT (appendSuffix wildcardPrefix name)
  else DatatypeT (Text.pack name) []
  where name = showSDocUnsafe $ ppr v
resolveType' t@(L _ HsAppTy{}) = DatatypeT (Text.pack dtName) dtArgs
 where
  dtName = case datatypeOf t of
    "[]"  -> "List"
    "(,)" -> "Pair"
    n     -> n
  dtArgs = datatypeArgs t

  datatypeOf :: LHsType GhcPs -> String
  datatypeOf (L _ (HsAppTy _ f _      )) = datatypeOf f
  datatypeOf (L _ (HsTyVar _ _ (L _ v))) = showSDocUnsafe (ppr v)
  datatypeOf _                           = error "unexpected datatype"

  datatypeArgs :: LHsType GhcPs -> [TypeSkeleton]
  datatypeArgs (L _ (HsAppTy _ (L _ HsTyVar{}) a)) = [resolveType' a]
  datatypeArgs (L _ (HsAppTy _ f a)) = datatypeArgs f ++ datatypeArgs a
  datatypeArgs t = [resolveType' t]

resolveType' (L _ (HsListTy _ t    )) = listType (resolveType' t)
resolveType' (L _ (HsTupleTy _ _ ts)) = foldr pairType basePair otherTyps
 where
  resolveTyps           = map resolveType' ts
  (baseTyps, otherTyps) = splitAt (length ts - 2) resolveTyps
  basePair              = pairType (head baseTyps) (last baseTyps)
resolveType' (L _ (HsParTy _ t)) = resolveType' t
resolveType' t                   = error $ showSDocUnsafe (ppr t)

antiSubstitute :: TypeSkeleton -> Id -> TypeSkeleton -> TypeSkeleton
antiSubstitute pat name t | t == pat = TypeVarT name
antiSubstitute pat name (DatatypeT dt args) =
  DatatypeT dt (map (antiSubstitute pat name) args)
antiSubstitute pat name (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
 where
  tArg' = antiSubstitute pat name tArg
  tRes' = antiSubstitute pat name tRes
antiSubstitute _ _ t = t

antiUnification
  :: MonadIO m
  => TypeSkeleton
  -> TypeSkeleton
  -> AntiUnifState
  -> TypeInferer m AntiUnifResult
antiUnification t1 t2 st = do
    -- liftIO $ print (t1, t2)
  (t, st) <- runStateT (antiUnification' t1 t2) st
  -- liftIO $ print t
  return (AntiUnifResult t st)

newAntiVariable
  :: MonadIO m
  => TypeSkeleton
  -> TypeSkeleton
  -> Maybe TyclassConstraints
  -> AntiUnifier m TypeSkeleton
newAntiVariable t1 t2 mbCons = do
    -- liftIO $ print "newAntiVariable start"
    -- liftIO $ print (t1, t2)
    -- liftIO $ print "newAntiVariable end"
    -- check whether the disunification is satisfiable
    -- liftIO $ print $ "Adding disunification constraint " ++ show (t1, t2)
  modify $ over disunifConstraints (DisunifConstraint t1 t2 :)
  checkConstraints
  -- if the disunification is satisfied, assign a new binding
  v <- lift $ lift $ freshId [] "t"
  modify $ over typeAssignment (Map.insert (t1, t2) v)
  when
    (isJust mbCons)
    ( modify
    $ over tyclassAssignment (Map.insertWith Set.union v (fromJust mbCons))
    )
  return $ TypeVarT v

-- there are two cases for create anti-unification variables
-- 1) consider reuse an existing binding if the sub unifies, add unification constraints
-- 2) consider create a new binding, add non-unification constraints
findWithDefaultAntiVariable
  :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifier m TypeSkeleton
findWithDefaultAntiVariable t1 t2 = do
  tass <- gets $ view typeAssignment
  -- liftIO $ print $ "current anti unification assignment " ++ show tass
  msum (map (unifiableVar (t1, t2)) (Map.toList tass))
    `mplus` -- reuse type variables
            (checkTyclass t1 t2 >>= newAntiVariable t1 t2) -- create new variables
 where
        -- we only allow unification between wildcards, not for anti-unification variables
  unifyAndCheck t1 t2 = do
    checkUnification t1 t2
    -- liftIO $ print $ "Adding unification constraint " ++ show (t1, t2)
    modify $ over unifConstraints (UnifConstraint t1 t2 :)
    checkConstraints
  unifiableVar (t1, t2) ((k1, k2), v) = do
    unifyAndCheck t1 k1
    unifyAndCheck t2 k2
    return (TypeVarT v)

-- there are two cases for antiUnification
-- 1) one of the type is a wildcard, return the other type
-- 2) either find an existing binding or create a new one
antiUnification'
  :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifier m TypeSkeleton
antiUnification' t1 t2 | t1 == t2 = return t1
antiUnification' TopT t           = return t
antiUnification' t    TopT        = return t
antiUnification' t1@(TypeVarT id1) t2@(TypeVarT id2)
  | hasWildcard t1
  = return (TypeVarT id2) `mplus` findWithDefaultAntiVariable t1 t2
  | hasWildcard t2
  = return (TypeVarT id1) `mplus` findWithDefaultAntiVariable t1 t2
antiUnification' t1@(TypeVarT id) t | hasWildcard t1 =
  return t `mplus` findWithDefaultAntiVariable t1 t
antiUnification' t t2@(TypeVarT id) | hasWildcard t2 =
  return t `mplus` findWithDefaultAntiVariable t t2
antiUnification' t1@(DatatypeT dt1 args1) t2@(DatatypeT dt2 args2)
  | dt1 == dt2 = do
      -- liftIO $ print "*************************************************"
      -- liftIO $ print (args1, args2)
    args' <- zipWithM antiUnification' args1 args2
    return (DatatypeT dt1 args')
antiUnification' (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
  tArg <- antiUnification' tArg1 tArg2
  tRes <- antiUnification' tRes1 tRes2
  return $ FunctionT x1 tArg tRes
antiUnification' t1 t2 = findWithDefaultAntiVariable t1 t2

generalizeType
  :: MonadIO m
  => [SchemaSkeleton]
  -> TyclassAssignment
  -> TypeSkeleton
  -> StateT TypeClassState m [(TyclassAssignment, TypeSkeleton)]
generalizeType exTyps tcass t = do
    -- do the filtering inside observeAll
  liftIO $ print t
  -- liftIO $ print tcass
  -- collect stats
  typs <- observeAllT
    (do
      let (_, vars) = partition isWildcard $ Set.toList $ freeVars t
      (ass, gt) <- evalStateT
        (generalizeTypeSkeleton tcass t)
        (TypeNaming Map.empty Map.empty Set.empty (Set.fromList vars))
      let (fvars, vars) = partition isWildcard $ Set.toList $ freeVars gt
      -- simplify computation here
      let mkSubst t = Map.fromList $ map (, t) fvars
      if null vars && null fvars
        then
          lift (modify $ over (infStats . prefilterCounts) (+ 1))
          >> lift (modify $ over (infStats . postfilterCounts) (+ 1))
          >> return (ass, gt)
        else msum $ map
          (\v -> do
            let t = apply (mkSubst (TypeVarT v)) gt
            lift $ modify $ over (infStats . prefilterCounts) (+ 1)
            -- liftIO $ print $ "Generalize into " ++ show t
            guard (isInhabited ass t)
            -- liftIO $ print "this is accepted"
            lift $ modify $ over (infStats . postfilterCounts) (+ 1)
            return (ass, t)
          )
          vars
    )
  return $ nubOrdBy dedupArgs typs

generalizeTypeSkeleton
  :: MonadIO m
  => TyclassAssignment
  -> TypeSkeleton
  -> TypeGeneralizer m (TyclassAssignment, TypeSkeleton)
generalizeTypeSkeleton tcass t@(TypeVarT id) = do
    -- either add type class or not
  let tc = maybe [] Set.toList (Map.lookup id tcass)
  let mkResult tc = return (Map.singleton id (Set.singleton tc), t)
  prevVars <- gets $ view prevTypeVars
  if id `Set.member` prevVars
    then return (Map.empty, t)
    else do
      modify $ over prevTypeVars (Set.insert id)
      msum (map mkResult tc) `mplus` return (Map.empty, t)
generalizeTypeSkeleton tcass t@(FunctionT x tArg tRes) = do
  (argTyclass, tArg') <- generalizeTypeSkeleton tcass tArg
  (resTyclass, tRes') <- generalizeTypeSkeleton tcass tRes
  let tyclassAssignment =
        Map.foldrWithKey (Map.insertWith Set.union) resTyclass argTyclass
  return (tyclassAssignment, FunctionT x tArg' tRes')
generalizeTypeSkeleton tcass t@(DatatypeT name args) =
  (do
      generalizedArgs <- mapM (generalizeTypeSkeleton tcass) args
      let (tyclassMaps, args') = unzip generalizedArgs
      let argTyclasses = foldr (Map.foldrWithKey (Map.insertWith Set.union))
                               Map.empty
                               tyclassMaps
      return (argTyclasses, DatatypeT name args')
    )
    `mplus` if null args || (plainShow t == "[Char]")
              then datatypeToVar tcass t
              else mzero
generalizeTypeSkeleton _ t = error $ "unsupported type " ++ plainShow t

datatypeToVar
  :: MonadIO m
  => TyclassAssignment
  -> TypeSkeleton
  -> TypeGeneralizer m (TyclassAssignment, TypeSkeleton)
datatypeToVar tcass t = do
    -- generalize the data type into a fresh type variable
    -- or reuse previous variables if tyclass checks
  tccache      <- lift $ lift $ gets $ view tyclassCache
  typeCounting <- gets $ view substCounter
  prevVars     <- gets $ view prevTypeVars
  beginVars    <- gets $ view beginTypeVars
  dtclasses    <- case Map.lookup t tccache of
    Just tc -> return tc
    Nothing -> lift . lift $ Set.toList <$> getDtTyclasses tcass t
  -- TODO: what is the good option to post-filter this cases
  -- because it is not always correct
  let matchedPrevVars = filter
        (\v -> case Map.lookup v tcass of
          Just tc ->
            not (Set.null (tc `Set.intersection` Set.fromList dtclasses))
          Nothing -> False
        )
        (Set.toList beginVars)
  let (v, startIdx) = case Map.lookup t typeCounting of
        Just (v, i) -> (v, i)
        Nothing ->
          let vars = Set.toList prevVars ++ map fst (Map.elems typeCounting)
              currMaxOrd = maximum (ord 'a' : map (ord . Text.head) vars)
          in  (Text.pack [chr (currMaxOrd + 1)], 0)
  -- choose between incr the index or not
  -- if reusing some previous var, do not backtrack over type classes
  msum
      (map
        (\v -> do
          modify $ over prevTypeVars (Set.insert v)
          return (Map.empty, TypeVarT v)
        )
        matchedPrevVars
      )
    `mplus` msum
              (map (\idx -> return (Map.empty, TypeVarT (appendIndex v idx)))
                   [1 .. startIdx]
              )
    `mplus`
      -- if start a new index
            if startIdx < 3 -- optimization for eval only
              then do
                let varName = appendIndex v (startIdx + 1)
                -- liftIO $ print varName
                modify $ over substCounter $ Map.insert t (v, startIdx + 1)
                modify $ over nameCounter $ Map.insert v (startIdx + 1)
                return (Map.empty, TypeVarT varName) `mplus` msum
                  (map
                    (\tc ->
                      return
                        ( Map.singleton varName (Set.singleton tc)
                        , TypeVarT varName
                        )
                    )
                    dtclasses
                  )
              else mzero

-- check the type class constraints for @t1@ and @t2@
-- return a new type variable with the calculated type classes
checkTyclass
  :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifier m (Maybe (Set Id))
checkTyclass (TypeVarT id1) (TypeVarT id2) = do
    -- assume both type variables cannot be freely unified
  tcass <- gets $ view tyclassAssignment
  let tc1 = Map.findWithDefault Set.empty id1 tcass
  let tc2 = Map.findWithDefault Set.empty id2 tcass
  let tc  = tc1 `Set.intersection` tc2
  return (if Set.null tc then Nothing else Just tc)
checkTyclass (TypeVarT id) t2 = do
    -- check the type class for the data type
  tcass   <- gets $ view tyclassAssignment
  tccache <- lift $ gets $ view tyclassCache
  let tc1 = Map.findWithDefault Set.empty id tcass
  tc <- case Map.lookup t2 tccache of
    Just tc2 -> return $ tc1 `Set.intersection` Set.fromList tc2
    Nothing  -> do
      tc2 <- lift . lift $ getDtTyclasses tcass t2
      return $ tc1 `Set.intersection` tc2
  return (if Set.null tc then Nothing else Just tc)
checkTyclass t1 t2@TypeVarT{} = checkTyclass t2 t1
checkTyclass t1 t2            = do
  tccache <- lift $ gets $ view tyclassCache
  tcass   <- gets $ view tyclassAssignment
  tc1     <- maybe (lift . lift $ getDtTyclasses tcass t1)
                   (return . Set.fromList)
                   (Map.lookup t1 tccache)
  tc2 <- maybe (lift . lift $ getDtTyclasses tcass t2)
               (return . Set.fromList)
               (Map.lookup t2 tccache)
  let tc = tc1 `Set.intersection` tc2
  return (if Set.null tc then Nothing else Just tc)

getDtTyclasses
  :: MonadIO m
  => TyclassAssignment
  -> TypeSkeleton
  -> StateT TypeClassState m TyclassConstraints
getDtTyclasses tcass typ = do
  mbCandidates <- mapM (mkTyclassQuery tcass typ) supportedTyclasses
  let tc = catMaybes mbCandidates
  modify $ over tyclassCache (Map.insert typ tc)
  return $ Set.fromList tc

mkTyclassQuery
  :: MonadIO m
  => TyclassAssignment
  -> TypeSkeleton
  -> Id
  -> StateT TypeClassState m (Maybe Id)
mkTyclassQuery tcass typ tyclass = do
  let vars = Set.toList $ freeVars typ
  -- this is unuseful now, consider removing it
  let varTcs = concatMap
        (\v -> case Map.lookup v tcass of
          Just tc -> map (v, ) $ Set.toList tc
          Nothing -> []
        )
        vars
  let varTcStrs = map (\(v, tc) -> Text.unwords [tc, v]) varTcs
  let allTcs =
        Text.intercalate ", "
          $  varTcStrs
          ++ [Text.pack $ printf "%s (%s)" tyclass (plainShow typ)]
  let query = printf "undefined :: (%s) => ()" allTcs
  liftIO $ catch
    (askGhc includedModules (exprType TM_Default query) >> return (Just tyclass)
    )
    (\(e :: SomeException) -> liftIO (print e) >> return Nothing)

defaultTypeVar :: TypeSkeleton -> TypeSkeleton -> TypeSkeleton
defaultTypeVar to t@(TypeVarT v) | hasWildcard t = to
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
typeVarPolarity reversed (TypeVarT v) = if reversed
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

isInhabited :: TyclassAssignment -> TypeSkeleton -> Bool
isInhabited tyclass t =
  let tcArgs = concatMap tyclassesToArgs $ Map.toList tyclass
      args   = map snd (argsWithName t)
      res    = lastType t
  in  isReachable args res && all (isRelevant (tcArgs ++ args) res) args
 where
  tyclassToArg v tc
    | tc == "Eq" || tc == "Ord" = FunctionT
      ""
      (TypeVarT v)
      (FunctionT "" (TypeVarT v) boolType)
    | tc == "Num" = FunctionT ""
                              (TypeVarT v)
                              (FunctionT "" (TypeVarT v) (TypeVarT v))
    | otherwise = error "Unsupported type class"

  tyclassesToArgs (v, tcs) =
    map (uncurry tyclassToArg . (v, )) $ Set.toList tcs

isReachable :: [TypeSkeleton] -> TypeSkeleton -> Bool
isReachable args res =
  freeVars res `Set.isSubsetOf` Set.unions (map freeVars args)

isRelevant :: [TypeSkeleton] -> TypeSkeleton -> TypeSkeleton -> Bool
isRelevant args res (TypeVarT id) =
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

dedupArgs
  :: (TyclassAssignment, TypeSkeleton)
  -> (TyclassAssignment, TypeSkeleton)
  -> Ordering
dedupArgs (tc1, t1) (tc2, t2) =
  let args1 = map snd (argsWithName t1)
      args2 = map snd (argsWithName t2)
      ret1  = lastType t1
      ret2  = lastType t2
  in  compare (sort args1, ret1, tc1) (sort args2, ret2, tc2)

reverseSubstitution
  :: TypeSkeleton
  -> TypeSkeleton
  -> Map TypeSkeleton (Set TypeSkeleton)
  -> Map TypeSkeleton (Set TypeSkeleton)
reverseSubstitution t1@TypeVarT{} t2 tass =
  Map.insertWith Set.union t1 (Set.singleton t2) tass
reverseSubstitution t1 t2@TypeVarT{} tass =
  Map.insertWith Set.union t1 (Set.singleton t2) tass
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

scoreSignature :: TypeSkeleton -> TyclassAssignment -> TypeSkeleton -> Double
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

checkConstraints :: MonadIO m => AntiUnifier m ()
checkConstraints = do
    -- first call type checker for unification constraints
    -- liftIO $ print "start unification"
  unifs <- gets $ view unifConstraints
  mapM_ checkConstraint unifs
  -- then call type checker for disunification constraints, with the previous unifier
  -- liftIO $ print "start disunification"
  disunifs <- gets $ view disunifConstraints
  mapM_ checkConstraint disunifs

checkConstraint :: MonadIO m => AntiUnifConstraint -> AntiUnifier m ()
checkConstraint (UnifConstraint t1 t2) = do
    -- liftIO (print "checkUnif")
    -- liftIO (print (t1, t2))
  checkUnification t1 t2
checkConstraint (DisunifConstraint t1 t2) = do
    -- liftIO $ print "checkDisunif"
  tass <- gets $ view tmpAssignment
  -- liftIO $ print tass
  let t1' = apply tass t1
  let t2' = apply tass t2
  -- liftIO $ print (t1', t2')
  checkDisunification t1' t2'
    -- liftIO $ print "end checkConstraint"

checkUnification
  :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifier m ()
checkUnification t1 t2 = do
    -- liftIO $ print "checkUnification"
    -- liftIO $ print (t1, t2)
  let vars      = Set.toList $ freeVars t1 `Set.union` freeVars t2
  let boundVars = filter isWildcard vars
  let env       = emptyEnv { getBoundTypeVars = boundVars }
  tmpAss <- gets $ view tmpAssignment
  -- liftIO $ print tmpAss
  -- liftIO $ print (p1, p2)
  let mbTass = checkTypes env vars t1 t2
  guard (isJust mbTass)
  modify $ set tmpAssignment (fromJust mbTass)
    -- liftIO $ print "checkUnification end"

checkDisunification
  :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifier m ()
checkDisunification t1 t2 | t1 == t2               = mzero
checkDisunification t@TypeVarT{} _ | hasWildcard t = return ()
checkDisunification _ t@TypeVarT{} | hasWildcard t = return ()
checkDisunification (DatatypeT dt1 args1) (DatatypeT dt2 args2)
  | dt1 /= dt2                             = return ()
  | dt1 == dt2 && null args1 && null args2 = mzero
  | otherwise                              = checkArgs args1 args2
 where
  checkArgs
    :: MonadIO m => [TypeSkeleton] -> [TypeSkeleton] -> AntiUnifier m ()
  checkArgs []             []             = mzero
  checkArgs (arg1 : args1) (arg2 : args2) = ifte
    (checkDisunification arg1 arg2)
    (const (return ()))
    (checkArgs args1 args2)
  checkArgs _ _ = error "checkDisunification: unexpected case"

checkDisunification (FunctionT _ tArg1 tRes1) (FunctionT _ tArg2 tRes2) = do
  checkDisunification tArg1 tArg2
  checkDisunification tRes1 tRes2
checkDisunification _ _ = return ()


------- Internal utilities

hasWildcard :: TypeSkeleton -> Bool
hasWildcard (DatatypeT _ args     ) = any hasWildcard args
hasWildcard (TypeVarT name        ) = wildcardPrefix == Text.take 1 name
hasWildcard (FunctionT _ tArg tRes) = hasWildcard tArg || hasWildcard tRes
hasWildcard _                       = False

isWildcard :: Id -> Bool
isWildcard name = wildcardPrefix == Text.take 1 name
