module HyperGraph.HyperGraph where

import Types.Abstract
import Types.Common

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)

type Edge = (Set AbstractSkeleton, Id, Set AbstractSkeleton)
type HyperGraph = Map (Set AbstractSkeleton) [Edge]