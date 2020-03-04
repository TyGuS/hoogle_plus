{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
module Types.Type where

import Types.Common
import Synquid.Logic

import GHC.Generics
import Data.Map (Map)
import Data.Aeson

data SchemaSkeleton r =
  Monotype (TypeSkeleton r) |
  ForallT Id (SchemaSkeleton r) | -- Type-polymorphic, each type variable may have some class constraints
  ForallP PredSig (SchemaSkeleton r)    -- Predicate-polymorphic
  deriving (Eq, Ord, Generic)

instance ToJSON r => ToJSON (SchemaSkeleton r)
instance FromJSON r => FromJSON (SchemaSkeleton r)

{- Type skeletons -}

data BaseType r = BoolT | IntT | DatatypeT Id [TypeSkeleton r] [r] | TypeVarT Substitution Id
  deriving (Eq, Ord, Generic)

instance ToJSON r => ToJSON (BaseType r)
instance FromJSON r => FromJSON (BaseType r)

-- | Type skeletons (parametrized by refinements)
data TypeSkeleton r =
  ScalarT (BaseType r) r |
  FunctionT Id (TypeSkeleton r) (TypeSkeleton r) |
  AnyT |
  BotT 
  deriving (Eq, Ord, Generic)

instance ToJSON r => ToJSON (TypeSkeleton r)
instance FromJSON r => FromJSON (TypeSkeleton r)

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
