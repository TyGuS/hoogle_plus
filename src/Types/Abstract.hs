{-# LANGUAGE DeriveGeneric  #-}
module Types.Abstract where

import Types.Common
import Types.Program
import Types.Type

import Data.Set (Set)
import GHC.Generics
import Data.Serialize
import Data.Hashable
import Data.Char

data AbstractBase = 
      ATypeVarT Id
    | ADatatypeT Id [AbstractSkeleton]
    deriving (Eq, Ord)

instance Hashable AbstractBase where
    hash (ATypeVarT id) = if isUpper (head id) then hash "" else hash id
    hash (ADatatypeT id args) = hash (intercalate "_" (id : map (show . hash) args))

data AbstractSkeleton =
      AScalar AbstractBase
    | AFunctionT AbstractSkeleton AbstractSkeleton
    deriving (Eq, Ord)

instance Hashable AbstractSkeleton where
    hash (AScalar b) = hash ("AScalar" ++ show (hash b))
    hash (AFunction arg res) = hash (show (hash arg) ++ "_" ++ show (hash res))

-- distinguish one type from a given general one
type SplitMsg = (AbstractSkeleton, AbstractSkeleton)

data SplitInfo = SplitInfo {
    splitedPlaces :: [(AbstractSkeleton, [AbstractSkeleton])],
    removedTrans :: [Id],
    newTrans :: [(Id, [Id])]
} deriving (Eq, Ord)

type AProgram = Program (RType, RType, AbstractSkeleton) 
-- (actual, expected, abstract) types
--
type UnifConstraint = (AbstractSkeleton, AbstractSkeleton)
