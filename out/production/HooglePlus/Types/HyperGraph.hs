module Types.HyperGraph where

import Types.Abstract
import Types.Common

import Data.Tree
import Data.Map.Strict (Map)

type PNState = [Int]
type StateTree = Tree (Either Int Id)

data Transition = Transition {
    transitionId :: Id,
    consumesFrom :: PNState,
    producesAt :: PNState
} deriving(Eq, Show)

data PetriNet = PetriNet {
    transitions :: Map Id Transition,
    consumptionTree :: StateTree,
    productionTree :: StateTree
} deriving(Eq, Show)