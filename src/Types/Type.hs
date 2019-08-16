{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
module Types.Type where

import Types.Common
import Synquid.Logic

import GHC.Generics
import Data.Map (Map)
import Data.Functor

data SchemaSkeleton r =
  Monotype (TypeSkeleton r) |
  ForallT Id (SchemaSkeleton r) | -- Type-polymorphic, each type variable may have some class constraints
  ForallP PredSig (SchemaSkeleton r)    -- Predicate-polymorphic
  deriving (Eq, Ord, Generic, Functor)

{- Type skeletons -}

data BaseType r = BoolT | IntT | DatatypeT Id [TypeSkeleton r] [r] | TypeVarT Substitution Id
  deriving (Eq, Ord, Generic, Functor)

-- | Type skeletons (parametrized by refinements)
data TypeSkeleton r =
  ScalarT (BaseType r) r |
  FunctionT Id (TypeSkeleton r) (TypeSkeleton r) |
  AnyT |
  BotT
  deriving (Eq, Ord, Generic, Functor)

-- | Unrefined typed
type SType = TypeSkeleton ()

-- | Refined types
type RType = TypeSkeleton Formula

-- | Unrefined schemas
type SSchema = SchemaSkeleton ()

-- | Refined schemas
type RSchema = SchemaSkeleton Formula

-- | Mapping from type variables to types
type TypeSubstitution = Map Id RType
