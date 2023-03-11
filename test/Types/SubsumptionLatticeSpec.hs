module Types.SubsumptionLatticeSpec
  ( spec
  ) where

import qualified Data.Map as Map

import Test.Hspec (Spec, describe, it, shouldBe)

import Types.Pretty
import Types.Type
import Types.TypeChecker
import Types.Substitution
import Types.Antiunifier
import Types.SubsumptionLattice

-- data LatticeTestcase = LatticeTestcase {
--   latticeDesc :: String,
--   lattice
-- }

data SubtractTestcase = SubtractTestcase {
  subtractDesc :: String,
  subtractLhs :: TypeSubstitution,
  subtractRhs :: TypeSubstitution,
  subtractWant :: TypeSubstitution
}

subtractTestcases :: [SubtractTestcase]
subtractTestcases = [
  SubtractTestcase {
    subtractDesc = "[x |-> Int, y |-> [Int]] - [y |-> [a]]",
    subtractLhs = Map.fromList [("x", intType), ("y", listType intType)],
    subtractRhs = Map.fromList [("y", listType (vart "a"))],
    subtractWant = Map.fromList [("x", intType), ("a", intType)]
  },
  SubtractTestcase {
    subtractDesc = "keep everything in su1 but not su2",
    subtractLhs = Map.fromList [("x", intType), ("y", listType intType)],
    subtractRhs = Map.fromList [("y", listType intType), ("z", boolType)],
    subtractWant = Map.fromList [("x", intType)]
  },
  SubtractTestcase {
    subtractDesc = "subtract under two wildcards",
    subtractLhs = Map.fromList [("x", listType intType), ("y", listType boolType)],
    subtractRhs = Map.fromList [("y", listType (vart "a")), ("x", listType (vart "b"))],
    subtractWant = Map.fromList [("a", boolType), ("b", intType)]
  }
  ]

spec :: Spec
spec = do
  describe "test subsumption lattice" $ return ()

  describe "test substitution subtraction" $
    mapM_ (\tc ->
      it (subtractDesc tc) $ do
        let result = subtractLhs tc `substSubtract` subtractRhs tc
        result `shouldBe` subtractWant tc
      ) subtractTestcases