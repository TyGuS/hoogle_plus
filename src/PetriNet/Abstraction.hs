module PetriNet.Abstraction where

import PetriNet.AbstractType
import Synquid.Type
import PetriNet.PNSolver

import qualified Data.Set as Set

firstLvAbs :: [RSchema] -> AbstractionTree
firstLvAbs schs =
    Set.foldl' updateSemantic (ALeaf (AExclusion Set.empty)) dts
  where
    typs = map (shape . toMonotype) schs
    dts = Set.unions (map allAbstractDts typs)
