module Types.Antiunifier
  (
    AntiunifState(..)
  , emptyAntiunifState
  , tyclAssignment
  , antiSubstitution
  , constraints
  , typeAssignment
  , Antiunifier
  , antiunify
  ) where

import Control.Monad.State
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust)

import Control.Monad.Logic

import Types.Common
import Types.Fresh
import Types.Substitution
import Types.Type
import Types.TypeChecker
import Types.TypeClass
import Examples.Utils

data AntiunifState = AntiunifState {
  _antiSubstitution :: Map (TypeSkeleton, TypeSkeleton) Id,
  _tyclAssignment   :: Map Id (Set Tycl),
  _constraints      :: [TypeConstraint],
  _typeAssignment   :: Map Id TypeSkeleton
} deriving Eq

makeLenses ''AntiunifState

emptyAntiunifState :: AntiunifState
emptyAntiunifState = AntiunifState Map.empty Map.empty [] Map.empty

type Antiunifier m = StateT AntiunifState (LogicT (MonadCounter (MonadTycl m)))

class (MonadIO m, Substitutable a) => Generalizable m a where
  -- | returns the most general unifier of two types or substitutions
  antiunify :: a -> a -> Antiunifier m a

instance MonadIO m => Generalizable m TypeSkeleton where
  -- there are two cases for antiUnification
  -- 1) one of the type is existential, return the other type
  -- 2) either find an existing binding or create a new one
  antiunify t1 t2 | t1 == t2 = return t1
  antiunify TopT t           = return t
  antiunify t    TopT        = return t
  antiunify t1@(TypeVarT Exists _) t2@(TypeVarT Exists _) = findWithDefaultAntiVariable t1 t2
  antiunify t1@(TypeVarT Exists _) t = return t `mplus` findWithDefaultAntiVariable t1 t
  antiunify t t2@(TypeVarT Exists _) = return t `mplus` findWithDefaultAntiVariable t t2
  antiunify t1@(DatatypeT dt1 args1) t2@(DatatypeT dt2 args2)
    | dt1 == dt2 = do args' <- zipWithM antiunify args1 args2
                      return (DatatypeT dt1 args')
  antiunify (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
    tArg <- antiunify tArg1 tArg2
    tRes <- antiunify tRes1 tRes2
    return $ FunctionT x1 tArg tRes
  antiunify t1 t2 = findWithDefaultAntiVariable t1 t2

newAntiVariable :: MonadIO m => TypeSkeleton -> TypeSkeleton -> Maybe (Set Tycl) -> Antiunifier m TypeSkeleton
newAntiVariable t1 t2 mbTycls = do
  -- Due to existential type variables,
  -- we need to ensure the disunification constraints are consistently satisfied.
  modify $ over constraints (DisunifiesWith t1 t2 :)
  checkConstraints
  -- if the disunification is satisfied, assign a new binding
  v <- lift $ lift $ freshId [] "t"
  modify $ over antiSubstitution (Map.insert (t1, t2) v)
  maybe (return ()) (\tycls -> modify $ over tyclAssignment (Map.insertWith Set.union v tycls)) mbTycls
  return $ vart v

-- there are two cases for create anti-unification variables
-- 1) consider reuse an existing binding if the sub unifies, add unification constraints
-- 2) consider create a new binding, add non-unification constraints
findWithDefaultAntiVariable :: MonadIO m => TypeSkeleton -> TypeSkeleton -> Antiunifier m TypeSkeleton
findWithDefaultAntiVariable t1 t2 = do
  antiSubst <- gets $ view antiSubstitution
  tyclAss <- gets $ view tyclAssignment
  msum (map (unifiableVar (t1, t2)) (Map.toList antiSubst)) -- reuse type variables
    `mplus` (fromMonadTycl (getTycl tyclAss t1 t2) >>= newAntiVariable t1 t2) -- create new variables
  where
    -- we only allow unification between existentials, not for anti-unification variables
    unifyAndCheck t1 t2 = do
      let vars = Set.toList $ freeVars t1 `Set.union` freeVars t2
      let mbTass = solveTypeConstraint vars Map.empty (UnifiesWith (skipTyclass t1) (skipTyclass t2))
      guard (isJust mbTass)
      modify $ over typeAssignment (`after` fromJust mbTass)

    unifiableVar (t1, t2) ((k1, k2), v) = do
      unifyAndCheck t1 k1
      unifyAndCheck t2 k2
      return (vart v)

checkConstraints :: MonadIO m => Antiunifier m ()
checkConstraints = gets (view constraints) >>= mapM_ checkConstraint

checkConstraint :: MonadIO m => TypeConstraint -> Antiunifier m ()
checkConstraint constraint = do
  tass <- gets $ view typeAssignment
  let constraint' = apply tass constraint
  let bvs = Set.toList (freeVars constraint)
  let mbTass = solveTypeConstraint bvs Map.empty constraint
  guard (isJust mbTass)

fromMonadTycl :: Monad m => MonadTycl m a -> Antiunifier m a
fromMonadTycl = lift . lift . lift