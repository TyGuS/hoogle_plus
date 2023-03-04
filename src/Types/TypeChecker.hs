module Types.TypeChecker
  (
    -- * Type checking
    CheckerState(..)
  , Checker
  , solveTypeConstraint
  , getUnifier
  , bottomUpCheck
  , emptyChecker

    -- * Type comparison
  , typeCmp
  , canonicalize

    -- * Abstract type comparison
  , isSubtypeOf
  , superTypeOf
  , equalAbstract
  , equalSplit
  , currentAbstraction
  , abstractIntersect
  , abstractApply
  , abstractStep
  , abstractCmp
  , existAbstract
  ) where

import           Control.Monad                  ( foldM )
import           Control.Monad.State            ( MonadState(get, put)
                                                , State
                                                , StateT
                                                , evalState
                                                , gets
                                                , modify
                                                , msum
                                                )
import Control.Monad.Except
import           Data.List.Extra                ( nubOrd )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text

import           Text.PrettyPrint.ANSI.Leijen   ( string )

import           Types.Common
import           Types.Environment
import           Types.Fresh
import           Types.Log
import           Types.Pretty
import           Types.Program hiding (canonicalize)
import           Types.Type
import           Utility.Utils

--------------------------------------------------------------------------------
------------------------------- Type checking ----------------------------------
--------------------------------------------------------------------------------

data CheckerState = CheckerState
  { getCounter        :: Map Id Int
  , getTypeAssignment :: TypeSubstitution
  , clogLevel         :: Int
  }
  deriving (Eq, Ord, Show)

emptyChecker :: CheckerState
emptyChecker = CheckerState Map.empty Map.empty 0

type Checker = State CheckerState

instance Monad m => Fresh CheckerState m where
  nextCounter prefix = do
    m <- gets getCounter
    let i = Map.findWithDefault 0 prefix m
    modify $ \s -> s { getCounter = Map.insert prefix (i + 1) m }
    return i

instance Loggable Checker where
  getLogLevel = gets clogLevel

-- bottom up check a program on the concrete type system
-- at the same time, keep track of the abstract type for each node
bottomUpCheck
  :: NameMapping
  -> Environment
  -> TProgram
  -> ExceptT TProgram Checker TProgram
bottomUpCheck nameMap env p@(Program (PSymbol sym) typ) = do
  let name = unsuffixName nameMap sym
  t <- lift $ findSymbol nameMap env name
  return $ Program (PSymbol sym) (toMonotype t)
bottomUpCheck nameMap env (Program (PApp f args) typ) = do
  checkedArgs <- mapM (bottomUpCheck nameMap env) args
  writeLog 3 "bottomUpCheck" $ text "Bottom up checking get arg types" <+> pretty (map typeOf checkedArgs)
  let name = unsuffixName nameMap f
  t <- lift $ findSymbol nameMap env name
  writeLog 3 "bottomUpCheck" $ text "Bottom up checking function" <+> pretty name <+> text "get type" <+> pretty t
  -- Check function signature against each argument provided.
  -- However, we cannot assume that the type of the function is already known.
  -- It may be a type variable or a TopT,
  -- in which case it unified with the target type even if the arity mismatches.
  tass <- gets getTypeAssignment
  freshRet <- lift (TypeVarT <$> freshId [] "T")
  let targetFunc = foldr (FunctionT "" . typeOf) freshRet checkedArgs
  let funcTass = solveTypeConstraint (getBoundTypeVars env) tass (UnifiesWith targetFunc $ toMonotype t)
  case funcTass of
    Nothing -> throwError $ Program (PApp f checkedArgs) BotT
    Just tm -> do modify $ \s -> s { getTypeAssignment = tm }
                  writeLog 3 "bottomUpCheck" $ text "Unified function type is" <+> pretty (typeSubstitute tm targetFunc) <+> text "with type assignment" <+> pretty tm
                  let args' = map (withContent (typeSubstitute tm)) checkedArgs
                  return $ Program (PApp f args') (typeSubstitute tm freshRet)
bottomUpCheck nameMap env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    body' <- bottomUpCheck nameMap (addComponent x (Monotype tArg) env) body
    tass <- gets getTypeAssignment
    let t     = FunctionT x (typeSubstitute tass tArg) (typeOf body')
    return $ Program (PFun x body') t
bottomUpCheck nameMap env p@(Program (PFun x body) _) = do
  writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
  let bound = getBoundTypeVars env
  tArg <- lift (TypeVarT <$> freshId bound "T")
  tRet <- lift (TypeVarT <$> freshId bound "T")
  bottomUpCheck nameMap env (Program (PFun x body) (FunctionT x tArg tRet))
bottomUpCheck _ _ p =
  error $ "unhandled case for checking " ++ show p ++ "::" ++ show (typeOf p)

--------------------------------------------------------------------------------
------------------------------- Type Relations ---------------------------------
--------------------------------------------------------------------------------

typeCmp :: TypeSkeleton -> TypeSkeleton -> Bool
typeCmp t1 t2 = canonicalize t1 == canonicalize t2

data CanonicalState = CanonicalState
  { varIndex :: Int
  , varMap   :: Map Id Id
  }

-- | turn polymorphic types into canonical forms
canonicalize :: TypeSkeleton -> TypeSkeleton
canonicalize t = evalState (canonicalize' t) (CanonicalState 0 Map.empty)

canonicalize' :: TypeSkeleton -> State CanonicalState TypeSkeleton
canonicalize' (TypeVarT var) = do
  CanonicalState idx m <- get
  case Map.lookup var m of
    Just var' -> return $ TypeVarT var'
    Nothing   -> do
      let var' = Text.pack $ "t" ++ show idx
      put $ CanonicalState (idx + 1) (Map.insert var var' m)
      return $ TypeVarT var'
canonicalize' (DatatypeT name tArgs) =
  DatatypeT name <$> mapM canonicalize' tArgs
canonicalize' (FunctionT x tArg tRes) = do
  tArg' <- canonicalize' tArg
  tRes' <- canonicalize' tRes
  return $ FunctionT x tArg' tRes'
canonicalize' t = return t

getUnifier :: [Id] -> [UnifConstraint] -> Maybe TypeSubstitution
getUnifier bvs = foldl (go bvs) (Just Map.empty)
 where
  go
    :: [Id]
    -> Maybe TypeSubstitution
    -> UnifConstraint
    -> Maybe TypeSubstitution
  go _   Nothing      _          = Nothing
  go bvs (Just subst) constraint = solveTypeConstraint bvs subst constraint

solveTypeConstraint
  :: [Id] -> TypeSubstitution -> UnifConstraint -> Maybe TypeSubstitution
-- subtype relationship
solveTypeConstraint _ subst (SubtypeOf t1 t2) | t1 == t2 = Just subst
solveTypeConstraint _ subst (SubtypeOf _ TopT)           = Just subst
solveTypeConstraint bvs subst (SubtypeOf TopT (TypeVarT v))
  | v `elem` bvs = Nothing
  | otherwise    = Just subst
solveTypeConstraint _ subst (SubtypeOf BotT _   ) = Just subst
solveTypeConstraint _ subst (SubtypeOf _    BotT) = Nothing
solveTypeConstraint bvs subst (SubtypeOf (TypeVarT v) t2) =
  case Map.lookup v subst of
    Just t  -> solveTypeConstraint bvs subst (SubtypeOf t t2)
    Nothing -> case t2 of
      TypeVarT v' -> case Map.lookup v' subst of
        Just t2' -> solveTypeConstraint bvs subst (SubtypeOf (TypeVarT v) t2')
        Nothing | v' `notElem` bvs -> unify subst v' (TypeVarT v)
                | otherwise        -> Nothing
      _ -> Nothing
solveTypeConstraint bvs subst (SubtypeOf t1 (TypeVarT v)) =
  case Map.lookup v subst of
    Just t -> solveTypeConstraint bvs subst (SubtypeOf t1 t)
    Nothing | v `elem` bvs -> Nothing
            | otherwise    -> unify subst v t1
solveTypeConstraint bvs subst (SubtypeOf (FunctionT _ tArg1 tRes1) (FunctionT _ tArg2 tRes2))
  = do
    subst' <- solveTypeConstraint bvs subst (SubtypeOf tArg1 tArg2)
    solveTypeConstraint bvs subst' (SubtypeOf tRes1 tRes2)
solveTypeConstraint bvs subst (SubtypeOf (DatatypeT dt1 args1) (DatatypeT dt2 args2))
  | dt1 == dt2 && length args1 == length args2
  = foldM
    (\subst' (t1, t2) -> solveTypeConstraint bvs subst' (SubtypeOf t1 t2))
    subst
    (zip args1 args2)
  | otherwise
  = Nothing
solveTypeConstraint _ _ (SubtypeOf _ _) = Nothing

-- unification constraints
solveTypeConstraint bvs subst (UnifiesWith t1 t2) =
  solveTypeConstraint' bvs subst t1 t2

solveTypeConstraint'
  :: [Id]
  -> TypeSubstitution
  -> TypeSkeleton
  -> TypeSkeleton
  -> Maybe TypeSubstitution
solveTypeConstraint' _ subst t1 t2 | t1 == t2      = Just subst
solveTypeConstraint' _   subst TopT           _    = Just subst
solveTypeConstraint' _   subst _              TopT = Just subst
solveTypeConstraint' _   subst BotT           _    = Nothing
solveTypeConstraint' _   subst _              BotT = Nothing
solveTypeConstraint' bvs subst (TypeVarT var) t2 = case Map.lookup var subst of
  Just t2' -> solveTypeConstraint' bvs subst t2' t2
  Nothing | var `elem` bvs -> solveTypeConstraint' bvs subst t2 (TypeVarT var)
          | otherwise      -> unify subst var t2
solveTypeConstraint' bvs subst t1 (TypeVarT var) = case Map.lookup var subst of
  Just t1' -> solveTypeConstraint' bvs subst t1 t1'
  Nothing | var `elem` bvs -> Nothing
          | otherwise      -> unify subst var t1
solveTypeConstraint' bvs subst (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet')
  = do
    subst' <- solveTypeConstraint' bvs subst tArg tArg'
    solveTypeConstraint' bvs subst' tRet tRet'
solveTypeConstraint' bvs subst (DatatypeT dt tArgs) (DatatypeT dt' tArgs')
  | dt /= dt' = Nothing
  | otherwise = solveArgConstraints subst tArgs tArgs'
 where
  solveArgConstraints
    :: TypeSubstitution
    -> [TypeSkeleton]
    -> [TypeSkeleton]
    -> Maybe TypeSubstitution
  solveArgConstraints st []             []               = Just st
  solveArgConstraints st (tArg : tArgs) (tArg' : tArgs') = do
    st' <- solveTypeConstraint' bvs st tArg tArg'
    solveArgConstraints st' tArgs tArgs'
  solveArgConstraints _ _ _ = Nothing
solveTypeConstraint' _ _ _ _ = Nothing

unify :: TypeSubstitution -> Id -> TypeSkeleton -> Maybe TypeSubstitution
unify subst x t | Set.member x (typeVarsOf t) = Nothing
                | isValidSubst subst && isValidSubst subst' = Just subst'
                | otherwise                   = Nothing
  where
    subst' = Map.insert x (typeSubstitute subst t) (Map.map (typeSubstitute $ Map.singleton x t) subst)

isValidSubst :: TypeSubstitution -> Bool
isValidSubst m =
  not $ any (\(v, t) -> v `Set.member` typeVarsOf t) (Map.toList m)


------------- Abstract type relations

-- this is subsumption relation, but not subtype relation!!!
isSubtypeOf :: [Id] -> TypeSkeleton -> TypeSkeleton -> Bool
isSubtypeOf _ t1 t2 | t1 == t2 = True
isSubtypeOf _     _    TopT    = True
isSubtypeOf _     BotT _       = True
isSubtypeOf bound t1   t2      = isJust unifier
 where
  unifier = getUnifier (bound ++ Set.toList (typeVarsOf t1)) [SubtypeOf t1 t2]

superTypeOf :: [Id] -> AbstractCover -> TypeSkeleton -> [TypeSkeleton]
superTypeOf tvs cover at = superTypeOf' tvs rootNode
 where
  superTypeOf' tvs paren =
    let children = Set.toList $ Map.findWithDefault Set.empty paren cover
    in  if isSubtypeOf tvs at paren
          then paren : concatMap (superTypeOf' tvs) children
          else []

equalAbstract :: [Id] -> TypeSkeleton -> TypeSkeleton -> Bool
equalAbstract bvs t1 t2 = isSubtypeOf bvs t1 t2 && isSubtypeOf bvs t2 t1

equalSplit :: [Id] -> SplitMsg -> SplitMsg -> Bool
equalSplit bvs s1 s2 = fst s1 == fst s2 && equalAbstract bvs (snd s1) (snd s2)

existAbstract :: [Id] -> AbstractCover -> TypeSkeleton -> Bool
existAbstract bvs cover t = existAbstract' rootNode
 where
  existAbstract' paren | equalAbstract bvs paren t = True
  existAbstract' paren | isSubtypeOf bvs t paren   = any
    existAbstract'
    (Set.toList $ Map.findWithDefault Set.empty paren cover)
  existAbstract' paren = False

abstractIntersect :: [Id] -> TypeSkeleton -> TypeSkeleton -> Maybe TypeSkeleton
abstractIntersect bound t1 t2 = case getUnifier bound [UnifiesWith t1 t2] of
  Nothing    -> Nothing
  Just subst -> Just $ typeSubstitute subst t1

-- | find the current most restrictive abstraction for a given type
currentAbstraction
  :: Fresh s m
  => [Id]
  -> AbstractCover
  -> TypeSkeleton
  -> StateT s m TypeSkeleton
currentAbstraction bvs cover (FunctionT x tArg tRes) = do
  tArg' <- currentAbstraction bvs cover tArg
  tRes' <- currentAbstraction bvs cover tRes
  return $ FunctionT x tArg' tRes'
currentAbstraction bvs cover at = do
  freshAt <- freshType bvs at
  case currentAbst' freshAt rootNode of
    Nothing -> error $ "cannot find current abstraction for type " ++ show at
    Just t  -> return t
 where
  currentAbst' at paren | isSubtypeOf bvs at paren =
    let children  = Set.toList $ Map.findWithDefault Set.empty paren cover
        inSubtree = any (isSubtypeOf bvs at) children
    in  if inSubtree then msum $ map (currentAbst' at) children else Just paren
  currentAbst' at paren = Nothing

abstractApply
  :: (Loggable (StateT s m), Fresh s m)
  => [Id]
  -> AbstractCover
  -> TypeSkeleton
  -> [TypeSkeleton]
  -> StateT s m TypeSkeleton
abstractApply bvs cover fun args = do
  let cargs = init (breakdown fun) -- higher-order arguments should have been transformed into funcTypes
  let ret   = last (breakdown fun)
  args' <- mapM (freshType bvs . toAbstractFun) args -- transform higher-order arguments
  writeLog 3 "abstractApply"
    $   "cargs:"
    <+> pretty cargs
    <+> ", args:"
    <+> pretty args'
  let unifier = getUnifier bvs (zipWith UnifiesWith cargs args')
  case unifier of
    Nothing -> return BotT
    Just m  -> do
      writeLog 3 "abstractApply" $ text "get unifier" <+> pretty (Map.toList m)
      let substRes = typeSubstitute m ret
      writeLog 3 "abstractApply" $ text "current cover" <+> string (show cover)
      currentAbstraction bvs cover substRes

abstractStep
  :: Fresh s m
  => [Id]
  -> AbstractCover
  -> TypeSkeleton
  -> TypeSkeleton
  -> StateT s m TypeSkeleton
abstractStep bvs cover (FunctionT _ tArg tRes) arg = do
  let unifier = getUnifier bvs [UnifiesWith tArg arg]
  case unifier of
    Nothing -> return BotT
    Just m  -> return $ typeSubstitute m tRes
abstractStep _ _ _ _ = return BotT

abstractCmp :: [Id] -> TypeSkeleton -> TypeSkeleton -> Ordering
abstractCmp bvs t1 t2 | isSubtypeOf bvs t1 t2 && isSubtypeOf bvs t2 t1 = EQ
                      | isSubtypeOf bvs t1 t2 = LT
                      | otherwise             = GT