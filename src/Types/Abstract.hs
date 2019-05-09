{-# LANGUAGE DeriveGeneric  #-}
module Types.Abstract where

import Types.Common
import Types.Program
import Types.Type

import Data.Set (Set)
import GHC.Generics
import Data.Serialize

-- | in case we need to add other kind of constraints
-- for now, we only have constraints with shape v ~ C t
data UnifConstraint = 
      TypeShape SType SType
    | NotShape SType SType
    deriving (Eq, Ord)

type Abstraction = Set UnifConstraint

data AbstractSkeleton =
      AScalar Abstraction
    | AFunctionT AbstractSkeleton AbstractSkeleton
    deriving (Eq, Ord)

data AbstractionTree =
      ALeaf Abstraction
    | ANode Abstraction AbstractionTree AbstractionTree
    deriving (Eq, Ord)

-- distinguish one type from a given general one
type SplitMsg = (AbstractSkeleton, AbstractSkeleton)

data SplitInfo = SplitInfo {
    splitedPlaces :: [(AbstractSkeleton, [AbstractSkeleton])],
    splitedGroup :: [(Id, [Id])]
} deriving (Eq, Ord)

type AProgram = Program (RType, RType, AbstractSkeleton) 
-- (actual, expected, abstract) types
