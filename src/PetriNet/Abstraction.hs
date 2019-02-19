module PetriNet.Abstraction where

import PetriNet.AbstractType
import Synquid.Type
import Synquid.Program
import PetriNet.PNSolver

import qualified Data.Set as Set
import Control.Lens

firstLvAbs :: Environment -> [RSchema] -> AbstractionTree
firstLvAbs env schs =
    Set.foldl' (updateSemantic env) (ALeaf (AExclusion Set.empty)) dts
  where
    typs = map (shape . toMonotype) schs
    dts = Set.unions (map (allAbstractDts (env ^. boundTypeVars)) typs)
