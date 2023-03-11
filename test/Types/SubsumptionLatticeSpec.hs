module Types.SubsumptionLatticeSpec
  ( spec
  ) where

import Control.Monad.State (modify)
import qualified Data.Map as Map

import Test.Hspec (Spec, describe, it, shouldBe)

import Types.Fresh
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

data InsertTestcase = InsertTestcase {
  insertDesc :: String,
  insertCounter :: Int,
  insertLattice :: Lattice,
  insertNode :: TypeSubstitution,
  insertWant :: Lattice
}

insertTestcases :: [InsertTestcase]
insertTestcases = [
  InsertTestcase {
    insertDesc = "insert into an empty lattice",
    insertCounter = 1,
    insertLattice = LatticeNode Map.empty [],
    insertNode = Map.fromList [("*0", pairType intType intType)],
    insertWant = LatticeNode Map.empty [LatticeNode (Map.fromList [("*0", pairType intType intType)]) []]
  },
  InsertTestcase {
    insertDesc = "insert into a subnode",
    insertCounter = 1,
    insertLattice = LatticeNode Map.empty [LatticeNode (Map.fromList [("*0", pairType intType intType)]) []],
    insertNode = Map.fromList [("*0", pairType intType boolType)],
    insertWant = LatticeNode Map.empty [
      LatticeNode (Map.fromList [("*0", pairType intType (vart "*1"))]) [
        LatticeNode (Map.fromList [("*1", intType)]) [],
        LatticeNode (Map.fromList [("*1", boolType)]) []
      ]]
  },
  InsertTestcase {
    insertDesc = "split a node",
    insertCounter = 2,
    insertLattice = LatticeNode Map.empty [
      LatticeNode (Map.fromList [("*0", pairType intType (vart "*1"))]) [
        LatticeNode (Map.fromList [("*1", intType)]) [],
        LatticeNode (Map.fromList [("*1", boolType)]) []
      ]],
    insertNode = Map.fromList [("*0", pairType (listType intType) charType)],
    insertWant = LatticeNode Map.empty [
      LatticeNode (Map.fromList [("*0", pairType (vart "*2") (vart "*1"))]) [
        LatticeNode (Map.fromList [("*2", intType)]) [
            LatticeNode (Map.fromList [("*1", intType)]) [],
            LatticeNode (Map.fromList [("*1", boolType)]) []
          ],
        LatticeNode (Map.fromList [("*2", listType intType), ("*1", charType)]) []
      ]
    ]
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

  describe "test lattice insertion" $
    mapM_ (\tc ->
      it (insertDesc tc) $ do
        result <- runMonadCounter $ modify (Map.insert "*" (insertCounter tc)) >> insert (insertNode tc) (insertLattice tc)
        result `shouldBe` insertWant tc
      ) insertTestcases