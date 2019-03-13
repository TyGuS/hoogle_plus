{-# LANGUAGE DeriveGeneric  #-}
module Types.Abstract where

import Synquid.Util

import Data.Set (Set)
import GHC.Generics

data AbstractSkeleton =
      ADatatypeT Id [AbstractSkeleton] -- explicit datatypes
    | AExclusion (Set Id) -- not included datatypes
    | ATypeVarT Id -- type variable is only temporarily before building the PetriNet
    | AFunctionT AbstractSkeleton AbstractSkeleton
    deriving (Eq, Ord, Show, Generic)

data AbstractionTree =
      ALeaf AbstractSkeleton
    | ANode AbstractSkeleton AbstractionTree AbstractionTree
    deriving (Eq, Ord, Show, Generic)
