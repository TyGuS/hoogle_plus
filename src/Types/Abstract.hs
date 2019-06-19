{-# LANGUAGE DeriveGeneric  #-}
module Types.Abstract where

import Types.Common
import Types.Program
import Types.Type

import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Data.Serialize
import Data.Char

data AbstractBase = 
      ATypeVarT Id
    | ADatatypeT Id [AbstractSkeleton]
    deriving (Eq, Ord)

data AbstractSkeleton =
      AScalar AbstractBase
    | AFunctionT AbstractSkeleton AbstractSkeleton
    | ABottom
    deriving (Eq, Ord)

-- distinguish one type from a given general one
type SplitMsg = (AbstractSkeleton, AbstractSkeleton)

data SplitInfo = SplitInfo {
    newPlaces :: [AbstractSkeleton],
    removedTrans :: [Id],
    newTrans :: [Id]
} deriving (Eq, Ord)

type AProgram = Program (RType, RType, AbstractSkeleton) 
-- (actual, expected, abstract) types
--
type UnifConstraint = (AbstractSkeleton, AbstractSkeleton)
