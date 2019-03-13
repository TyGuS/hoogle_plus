module PetriNet.Abstraction where

import PetriNet.AbstractType
import Synquid.Type
import Types.Environment
import PetriNet.PNSolver
import Synquid.Util

import qualified Data.Set as Set
import Control.Lens

firstLvAbs :: Environment -> [RSchema] -> AbstractionTree
firstLvAbs env schs =
    Set.foldl' (updateSemantic env) (ALeaf (AExclusion Set.empty)) dts
  where
    typs = map (shape . toMonotype) schs
    dts = Set.unions (map (allAbstractDts (env ^. boundTypeVars)) typs)


-- Produce the most specific abstraction possible from the given types.
specificAbstractionFromTypes :: Environment -> [RSchema] -> AbstractionTree
specificAbstractionFromTypes env schemas = let
    abstrSkels = map (toAbstractType . shape . toMonotype) schemas
    baseTree = ALeaf (AExclusion Set.empty)
    in
        foldr (flip $ updateSemantic env) baseTree abstrSkels
