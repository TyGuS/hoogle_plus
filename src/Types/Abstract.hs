{-# LANGUAGE DeriveGeneric  #-}
module Types.Abstract where

import Types.Common

import Data.Set (Set)
import GHC.Generics
import Data.Serialize

data AbstractSkeleton =
      ADatatypeT Id [AbstractSkeleton] -- explicit datatypes
    | AExclusion (Set Id) -- not included datatypes
    | ATypeVarT Id -- type variable is only temporarily before building the PetriNet
    | AFunctionT AbstractSkeleton AbstractSkeleton
    deriving (Eq, Ord, Show, Generic)

instance Serialize AbstractSkeleton

data AbstractionTree =
      ALeaf AbstractSkeleton
    | ANode AbstractSkeleton AbstractionTree AbstractionTree
    deriving (Eq, Ord, Show, Generic)

-- distinguish one type from a given general one
type SplitMsg = (AbstractSkeleton, AbstractSkeleton)

data SplitInfo = SplitInfo {
    splitedPlaces :: [(AbstractSkeleton, [AbstractSkeleton])],
    splitedGroup :: [(Id, [Id])]
} deriving (Eq, Ord, Show)
