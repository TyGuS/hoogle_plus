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

import           Control.Lens                   ( (^.)
                                                , view
                                                )
import           Control.Monad.State            ( MonadState(get, put)
                                                , State
                                                , StateT
                                                , evalState
                                                , gets
                                                , modify
                                                , msum
                                                )
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
import           Types.Pretty
import           Types.Program
import           Types.Type
import           Utility.Utils

--------------------------------------------------------------------------------
------------------------------- Type checking ----------------------------------
--------------------------------------------------------------------------------

data CheckerState = CheckerState
  { getCounter        :: Map Id Int
  , getTypeAssignment :: TypeSubstitution
  }
  deriving (Eq, Ord, Show)

emptyChecker :: CheckerState
emptyChecker = CheckerState Map.empty Map.empty

type Checker = State CheckerState

instance Monad m => Fresh CheckerState m where
  nextCounter prefix = do
    m <- gets getCounter
    let i = Map.findWithDefault 0 prefix m
    modify $ \s -> s { getCounter = Map.insert prefix (i + 1) m }
    return i

-- bottom up check a program on the concrete type system
-- at the same time, keep track of the abstract type for each node
bottomUpCheck
  :: NameMapping
  -> Environment
  -> TProgram
  -> Checker (Either TProgram TProgram)
bottomUpCheck nameMap env p@(Program (PSymbol sym) typ) = do
  let sym' = removeLast '_' sym
  let name = stripSuffix hoPostfix $ fromMaybe sym' (Map.lookup sym' nameMap)
  t <- findSymbol nameMap env name
  return $ Right $ Program (PSymbol sym) (toMonotype t)
bottomUpCheck nameMap env (Program (PApp f args) typ) = do
  -- this is bottom up type checking, so we check the arguments first
  argResult <- checkArgs args
  case argResult of
    Left  err         -> return $ Left err
    Right checkedArgs -> do
      let f'   = removeLast '_' f
      let name = stripSuffix hoPostfix $ fromMaybe f' (Map.lookup f' nameMap)
      t <- findSymbol nameMap env name
      writeLog 3 "bottomUpCheck"
        $   text "Bottom up checking function"
        <+> pretty f
        <+> text "get type"
        <+> pretty t
      -- check function signature against each argument provided
      let argVars       = allArgTypes (toMonotype t)
      let checkedArgTys = map typeOf checkedArgs
      writeLog 3 "bottomUpCheck"
        $   text "Bottom up checking get arg types"
        <+> pretty checkedArgTys
      tass <- gets getTypeAssignment
      let tass' = foldl (uncurry . solveArgConstraint (getBoundTypeVars env))
                        (Just tass)
                        (zip checkedArgTys argVars)
      case tass' of
        -- if any of these checks returned false, this function application
        -- would produce a bottom type
        Nothing -> return $ Right $ Program (PApp f checkedArgs) BotT
        Just tm -> do
          modify $ \s -> s { getTypeAssignment = tm }
          -- we eagerly substitute the assignments into the return type of t
          let ret =
                typeSubstitute tm (partialReturn checkedArgs $ toMonotype t)
          return $ Right $ Program (PApp f checkedArgs) ret
 where
  partialReturn :: [TProgram] -> TypeSkeleton -> TypeSkeleton
  partialReturn (_ : args) (FunctionT _ _ tRes) = partialReturn args tRes
  partialReturn [] t = t
  partialReturn _ _ = error "partialReturn: not a function"

  checkArgs :: [TProgram] -> Checker (Either TProgram [TProgram])
  checkArgs []           = return $ Right []
  checkArgs (arg : args) = do
    checkedArg <- bottomUpCheck nameMap env arg
    case checkedArg of
      Left  arg' -> return $ Left arg'
      Right arg' -> do
        checkedArgs <- checkArgs args
        case checkedArgs of
          Left  err   -> return $ Left err
          Right args' -> return $ Right (arg' : args')

  solveArgConstraint
    :: [Id]
    -> Maybe TypeSubstitution
    -> TypeSkeleton
    -> TypeSkeleton
    -> Maybe TypeSubstitution
  solveArgConstraint _ Nothing _ _ = Nothing
  solveArgConstraint bvs (Just tass) t1 t2 =
    solveTypeConstraint bvs tass (SubtypeOf t1 t2)

bottomUpCheck nameMap env p@(Program (PFun x body) (FunctionT _ tArg tRet)) =
  do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    checkedBody <- bottomUpCheck nameMap
                                 (addComponent x (Monotype tArg) env)
                                 body
    case checkedBody of
      Left  err   -> return $ Left err
      Right body' -> do
        let tBody = typeOf body'
        let t     = FunctionT x tArg tBody
        return $ Right $ Program (PFun x body') t
bottomUpCheck nameMap env p@(Program (PFun x body) _) = do
  writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
  let bound = getBoundTypeVars env
  tArg <- TypeVarT <$> freshId bound "T"
  tRet <- TypeVarT <$> freshId bound "T"
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
  go _ Nothing _ = Nothing
  go bvs (Just subst) (SubtypeOf t1 t2) =
    solveTypeConstraint bvs subst (SubtypeOf t1 t2)

solveTypeConstraint
  :: [Id] -> TypeSubstitution -> UnifConstraint -> Maybe TypeSubstitution
solveTypeConstraint bvs subst (SubtypeOf t1 t2) =
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
  Nothing | var `elem` bvs -> Nothing
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
  where subst' = Map.insert x (typeSubstitute subst t) subst

isValidSubst :: TypeSubstitution -> Bool
isValidSubst m =
  not $ any (\(v, t) -> v `Set.member` typeVarsOf t) (Map.toList m)


------------- Abstract type relations

-- this is subsumption relation, but not subtype relation!!!
isSubtypeOf :: [Id] -> TypeSkeleton -> TypeSkeleton -> Bool
isSubtypeOf _ t1 t2 | t1 == t2 = True
isSubtypeOf _     _    TopT    = True
isSubtypeOf _     TopT _       = False
isSubtypeOf _     BotT _       = True
isSubtypeOf _     _    BotT    = False
isSubtypeOf bound t1   t2      = isJust unifier
 where
  unifier = getUnifier (bound ++ Set.toList (typeVarsOf t1)) [SubtypeOf t1 t2]

superTypeOf :: [Id] -> AbstractCover -> TypeSkeleton -> [TypeSkeleton]
superTypeOf tvs cover at = superTypeOf' tvs rootNode
  where
    superTypeOf' tvs paren = let
        children = Set.toList $ Map.findWithDefault Set.empty paren cover
        in if isSubtypeOf tvs at paren then paren : concatMap (superTypeOf' tvs) children
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
abstractIntersect bound t1 t2 = case getUnifier bound [SubtypeOf t1 t2] of
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
  :: Fresh s m
  => [Id]
  -> AbstractCover
  -> TypeSkeleton
  -> [TypeSkeleton]
  -> StateT s m TypeSkeleton
abstractApply bvs cover fun args = do
  let cargs = init (breakdown fun) -- higher-order arguments should have been transformed into funcTypes
  let ret   = last (breakdown fun)
  args' <- mapM (freshType bvs . toAbstractFun) args -- transform higher-order arguments
  let unifier = getUnifier bvs (zipWith SubtypeOf cargs args')
  case unifier of
    Nothing -> return BotT
    Just m  -> do
      writeLog 3 "abstractApply" $ text "get unifier" <+> pretty (Map.toList m)
      let substRes = typeSubstitute m ret
      writeLog 3 "abstractApply" $ text "current cover" <+> string (show cover)
      currentAbstraction bvs cover substRes

abstractStep :: Fresh s m => [Id] -> AbstractCover -> TypeSkeleton -> TypeSkeleton -> StateT s m TypeSkeleton
abstractStep bvs cover (FunctionT _ tArg tRes) arg = do
  tArg' <- freshType bvs $ toAbstractFun tArg
  let unifier = getUnifier bvs [SubtypeOf tArg' arg]
  case unifier of
    Nothing -> return BotT
    Just m -> currentAbstraction bvs cover (typeSubstitute m tRes)
abstractStep _ _ _ _ = return BotT

abstractCmp :: [Id] -> TypeSkeleton -> TypeSkeleton -> Ordering
abstractCmp bvs t1 t2 | isSubtypeOf bvs t1 t2 && isSubtypeOf bvs t2 t1 = EQ
                      | isSubtypeOf bvs t1 t2 = LT
                      | otherwise             = GT