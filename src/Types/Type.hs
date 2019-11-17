{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
module Types.Type where

import Types.Common

import GHC.Generics
import Data.Map (Map)


data SchemaSkeleton =
  Monotype TypeSkeleton |
  ForallT (Id, Kind) SchemaSkeleton -- Type-polymorphic, each type variable may have some class constraints
  deriving (Eq, Ord, Generic)

{- Type kind -}
data Kind = KnStar | KnArr Kind Kind
  deriving (Eq, Ord, Generic)

{- Type skeletons -}
data TypeSkeleton =
  TypeVarT Id |
  DatatypeT Id |
  TyFunT TypeSkeleton TypeSkeleton |
  TyAppT TypeSkeleton TypeSkeleton |
  FunctionT Id TypeSkeleton TypeSkeleton |
  AnyT |
  BotT 
  deriving (Eq, Ord, Generic)

{- Type synonyms -}
-- | Mapping from type variables to types
type TypeSubstitution = Map Id TypeSkeleton
-- second order kind
type KnFst = KnArr KnStar KnStar
-- third order kind
type KnSec = KnArr KnStar (KnArr KnStar KnStar)