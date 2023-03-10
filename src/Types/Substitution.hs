module Types.Substitution
  ( Substitutable(..)
  , TypeSubstitution
  , forall
  , after
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Types.Common
import Types.Type

type TypeSubstitution = Map Id TypeSkeleton

class Substitutable a where
  apply :: TypeSubstitution -> a -> a
  freeVars :: a -> Set Id

instance Substitutable TypeSkeleton where
  apply subst t@(TypeVarT _ id) = Map.findWithDefault t id subst
  apply subst (DatatypeT name tArgs) = DatatypeT name (map (apply subst) tArgs)
  apply subst (FunctionT x tArg tRes) = FunctionT x (apply subst tArg) (apply subst tRes)
  apply subst t = t

  freeVars (TypeVarT Exists name) = Set.empty -- zheng: is this correct?
  freeVars (TypeVarT _ name) = Set.singleton name
  freeVars (DatatypeT _ tArgs) = Set.unions (map freeVars tArgs)
  freeVars (FunctionT _ tArg tRes) = freeVars tArg `Set.union` freeVars tRes
  freeVars _ = Set.empty

instance Substitutable SchemaSkeleton where
  apply subst (Monotype t) = Monotype $ apply subst t
  apply subst (ForallT a sch) = ForallT a $ apply (Map.delete a subst) sch

  freeVars (Monotype t) = freeVars t
  freeVars (ForallT a sch) = Set.delete a (freeVars sch)

instance Substitutable TypeConstraint where
  apply subst (UnifiesWith t1 t2) = UnifiesWith (apply subst t1) (apply subst t2)
  apply subst (DisunifiesWith t1 t2) = DisunifiesWith (apply subst t1) (apply subst t2)
  apply subst (SubtypeOf t1 t2) = SubtypeOf (apply subst t1) (apply subst t2)

  freeVars (UnifiesWith t1 t2) = freeVars t1 `Set.union` freeVars t2
  freeVars (DisunifiesWith t1 t2) = freeVars t1 `Set.union` freeVars t2
  freeVars (SubtypeOf t1 t2) = freeVars t1 `Set.union` freeVars t2

forall :: TypeSkeleton -> SchemaSkeleton
forall t = foldr ForallT (Monotype t) (freeVars t)

after :: TypeSubstitution -> TypeSubstitution -> TypeSubstitution
after subst1 subst2 = (Map.map (apply subst1) subst2) `Map.union` subst1