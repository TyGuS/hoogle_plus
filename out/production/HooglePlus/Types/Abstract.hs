{-# LANGUAGE DeriveGeneric #-}

module Types.Abstract where

import Types.Common
import Types.Program
import Types.Type

import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Data.Serialize
import Data.Char
import Data.Hashable
import Data.Function

data AbstractBase =
      ATypeVarT Id
    | ADatatypeT Id [AbstractSkeleton]
    deriving (Eq, Ord, Generic)

data AbstractSkeleton =
      AScalar AbstractBase
    | AFunctionT AbstractSkeleton AbstractSkeleton
    | ACompound [AbstractSkeleton]
    | ABottom
    deriving (Eq, Ord, Generic)

instance Hashable AbstractBase
instance Hashable AbstractSkeleton

-- distinguish one type from a given general one
type SplitMsg = (AbstractSkeleton, AbstractSkeleton)

data SplitInfo = SplitInfo {
    newPlaces :: [Int],
    removedTrans :: [Id],
    newTrans :: [Id]
} deriving (Eq, Ord)

type AProgram = Program (RType, RType, AbstractSkeleton)
-- (actual, expected, abstract) types
--
type UnifConstraint = (AbstractSkeleton, AbstractSkeleton)

data FunctionCode = FunctionCode {
  funName   :: String,  -- function name
  hoParams  :: [FunctionCode],
  funParams :: [Int], -- function parameter types and their count
  funReturn :: [Int]   -- function return type
}

instance Eq FunctionCode where
  fc1 == fc2 = let
    areEq arg = on (==) arg fc1 fc2
    in areEq hoParams && areEq funParams && areEq funReturn

instance Ord FunctionCode where
  compare fc1 fc2 = let
    thenCmp EQ       ordering = ordering
    thenCmp ordering _        = ordering
    cmp arg = on compare arg fc1 fc2
    in foldr1 thenCmp [cmp hoParams, cmp funParams, cmp funReturn]
