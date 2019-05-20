{-# LANGUAGE DeriveGeneric  #-}
module Types.Abstract where

import Types.Common

import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Data.Serialize
import Data.Tree
import Data.Tree.Pretty
import Data.List.Extra (intercalate)
import Text.Printf

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

prettySkeleton (ADatatypeT id sks) | length sks > 1 = unwords $ id:(map (\x -> printf "(%s)" $ prettySkeleton x) sks)
prettySkeleton (ADatatypeT id sks) = unwords $ id:(map prettySkeleton sks)
prettySkeleton (AExclusion ids) | null ids = "*"
prettySkeleton (AExclusion ids) = "¬{" ++ (intercalate ", " (Set.toList ids)) ++ "}"
prettySkeleton (ATypeVarT id) = id
prettySkeleton (AFunctionT args ret) = prettySkeleton args ++ " → " ++ prettySkeleton ret

prettyPrintAbstractionTree x = drawVerticalTree $ (prettySkeleton <$> (abstractTreetoTree x))
  where
    abstractTreetoTree :: AbstractionTree -> Tree AbstractSkeleton
    abstractTreetoTree (ALeaf sk) = Node sk []
    abstractTreetoTree (ANode sk left right) = Node sk [abstractTreetoTree left, abstractTreetoTree right]