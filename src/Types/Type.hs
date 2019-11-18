{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
module Types.Type where

import Types.Common

import GHC.Generics
import Data.Map (Map)


data SchemaSkeleton =
  Monotype TypeSkeleton |
  ForallT Id SchemaSkeleton -- Type-polymorphic, each type variable may have some class constraints
  deriving (Eq, Ord, Generic)

{- Type kind -}
data Kind = KnStar | KnArr Kind Kind

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
knFst = KnArr KnStar KnStar
-- third order kind
knSec = KnArr KnStar (KnArr KnStar KnStar)