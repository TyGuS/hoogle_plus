{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Types.Type where

import Synquid.Logic
import Types.Common

import Data.Map (Map)
import GHC.Generics

data SchemaSkeleton r
    = Monotype (TypeSkeleton r)
    | ForallT Id (SchemaSkeleton r) -- Type-polymorphic, each type variable may have some class constraints
    | ForallP PredSig (SchemaSkeleton r) -- Predicate-polymorphic
    deriving (Eq, Ord, Generic)

{- Type skeletons -}
data BaseType r
    = BoolT
    | IntT
    | DatatypeT Id [TypeSkeleton r] [r]
    | TypeVarT Substitution Id
    deriving (Eq, Ord, Generic)

-- | Type skeletons (parametrized by refinements)
data TypeSkeleton r
    = ScalarT (BaseType r) r
    | FunctionT Id (TypeSkeleton r) (TypeSkeleton r)
    | AnyT
    | BotT
    deriving (Eq, Ord, Generic)

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
