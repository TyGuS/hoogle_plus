{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
module Types.Type where

import Types.Common

import GHC.Generics
import Data.Map (Map)
import Data.Aeson
import Data.Hashable

data SchemaSkeleton =
  Monotype TypeSkeleton |
  ForallT Id SchemaSkeleton -- Type-polymorphic, each type variable may have some class constraints
  deriving (Eq, Ord, Generic)

instance ToJSON SchemaSkeleton
instance FromJSON SchemaSkeleton

{- Type kind -}
data Kind = KnVar Id | KnAny | KnStar | KnArr Kind Kind
  deriving (Eq, Ord, Generic)

{- Type skeletons -}
data TypeSkeleton =
  TypeVarT Id |
  DatatypeT Id |
  TyFunT TypeSkeleton TypeSkeleton |
  TyAppT TypeSkeleton TypeSkeleton |
  FunctionT Id TypeSkeleton TypeSkeleton |
  AnyT |
  BottomT 
  deriving (Eq, Ord, Generic)

instance ToJSON TypeSkeleton
instance FromJSON TypeSkeleton
instance Hashable TypeSkeleton

-- distinguish one type from a given general one
type SplitMsg = (TypeSkeleton, TypeSkeleton)

data SplitInfo = SplitInfo {
    newPlaces :: [TypeSkeleton],
    removedTrans :: [Id],
    newTrans :: [Id]
} deriving (Eq, Ord)

{- Type synonyms -}
-- abstract type can be represented in the same data structure
type AbstractSkeleton = TypeSkeleton
type AbsArguments = [AbstractSkeleton]
type AbsReturn = [AbstractSkeleton]
-- unification constraints
type UnifConstraint = (TypeSkeleton, TypeSkeleton)
-- | Mapping from type variables to types
type TypeSubstitution = Map Id TypeSkeleton
-- second order kind
knFst = KnArr KnStar KnStar
-- third order kind
knSec = KnArr KnStar (KnArr KnStar KnStar)