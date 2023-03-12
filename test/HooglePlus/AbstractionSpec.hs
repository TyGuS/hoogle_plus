module HooglePlus.AbstractionSpec
  (
    spec
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Hspec (Spec, describe, it, shouldBe)

import Types.Common
import Types.Pretty
import HooglePlus.Abstraction

data TypeLevelTestcase = TypeLevelTestcase {
  tlvDesc :: String,
  tlvDatatypes :: [(Id, Int)],
  tlvLevel :: Int,
  tlvWant :: [String]
}

typeDepthTestcases :: [TypeLevelTestcase]
typeDepthTestcases = [
  TypeLevelTestcase {
    tlvDesc = "generate types at level 0",
    tlvDatatypes = [("Int", 0), ("Bool", 0), ("List", 1), ("Pair", 2)],
    tlvLevel = 0,
    tlvWant = ["Int", "Bool"]
  },
  TypeLevelTestcase {
    tlvDesc = "generate types at level 1",
    tlvDatatypes = [("Int", 0), ("Bool", 0), ("List", 1), ("Pair", 2)],
    tlvLevel = 1,
    tlvWant = ["[Int]", "[Bool]", "(Int, Int)", "(Int, Bool)", "(Bool, Bool)", "(Bool, Int)", "[_v1]", "(_v1, _v2)"]
  },
  TypeLevelTestcase {
    tlvDesc = "generate types at level 2",
    tlvDatatypes = [("Int", 0), ("Bool", 0), ("List", 1), ("Pair", 2)],
    tlvLevel = 2,
    tlvWant = ["[[Int]]", "[[Bool]]", "[(Int, Int)]", "(Int, [_v1])", "(Int, (_v1, _v2))","[(Int, Bool)]", "[(Bool, Bool)]", "[(Bool, Int)]", "[[_v1]]", "[(_v1, _v2)]",
               "(Int, [Int])", "(Int, [Bool])", "(Int, (Int, Int))", "(Int, (Int, Bool))", "(Int, (Bool, Bool))", "(Int, (_v1, _v2))","(Int, (Bool, Int))",
               "([Int], [Int])", "([Int], [_v1])", "([Int], (_v1, _v2))", "([Int], Int)", "([Int], Bool)", "([Int], [Bool])", "([Int], (Int, Int))", "([Int], (_v1, _v2))","([Int], (Int, Bool))", "([Int], (Bool, Bool))", "([Int], (Bool, Int))",
               "(Bool, [Int])", "(Bool, [_v1])","(Bool, [Bool])", "(Bool, (Int, Int))", "(Bool, (Int, Bool))", "(Bool, (_v1, _v2))","(Bool, (Bool, Bool))", "(Bool, (Bool, Int))",
               "([Bool], [Int])", "([Bool], [_v1])","([Bool], [Bool])", "([Bool], Int)", "([Bool], Bool)", "([Bool], (_v1, _v2))", "([Bool], (Int, Int))", "([Bool], (Int, Bool))", "([Bool], (Bool, Bool))", "([Bool], (Bool, Int))",
               "((Int, Int), [Int])", "((Int, Int), [_v1])","((Int, Int), Int)","((Int, Int), Bool)","((Int, Int), (_v1, _v2))","((Int, Int), [Bool])", "((Int, Int), (Int, Int))", "((Int, Int), (Int, Bool))", "((Int, Int), (Bool, Bool))", "((Int, Int), (Bool, Int))",
               "((Int, Bool), [Int])", "((Int, Bool), [_v1])","((Int, Bool), Int)", "((Int, Bool), (_v1, _v2))","((Int, Bool), Bool)","((Int, Bool), [Bool])", "((Int, Bool), (Int, Int))", "((Int, Bool), (Int, Bool))", "((Int, Bool), (Bool, Bool))", "((Int, Bool), (Bool, Int))",
               "((Bool, Bool), [Int])", "((Bool, Bool), [_v1])","((Bool, Bool), (_v1, _v2))", "((Bool, Bool), Int)","((Bool, Bool), Bool)","((Bool, Bool), [Bool])", "((Bool, Bool), (Int, Int))", "((Bool, Bool), (Int, Bool))", "((Bool, Bool), (Bool, Bool))", "((Bool, Bool), (Bool, Int))",
               "((Bool, Int), [Int])", "((Bool, Int), [_v1])", "((Bool, Int), (_v1, _v2))","((Bool, Int), Int)","((Bool, Int), Bool)","((Bool, Int), [Bool])", "((Bool, Int), (Int, Int))", "((Bool, Int), (Int, Bool))", "((Bool, Int), (Bool, Bool))", "((Bool, Int), (Bool, Int))",
               "([_v1], (_v1, _v2))", "([_v1], [_v1])", "([_v1], [Int])", "([_v1], Int)", "([_v1], Bool)", "([_v1], [Bool])", "([_v1], (Int, Int))", "([_v1], (_v1, _v2))","([_v1], (Int, Bool))", "([_v1], (Bool, Bool))", "([_v1], (Bool, Int))",
               "((_v1, _v2), (_v1, _v2))", "((_v1, _v2), (_v1, _v2))", "((_v1, _v2), [_v1])", "((_v1, _v2), [Int])", "((_v1, _v2), Int)", "((_v1, _v2), Bool)", "((_v1, _v2), [Bool])", "((_v1, _v2), (Int, Int))", "((_v1, _v2), (_v1, _v2))","((_v1, _v2), (Int, Bool))", "((_v1, _v2), (Bool, Bool))", "((_v1, _v2), (Bool, Int))"
               ]
  }
  ]

typeSizeTestcases :: [TypeLevelTestcase]
typeSizeTestcases = [
  TypeLevelTestcase {
    tlvDesc = "generate types of size 1",
    tlvDatatypes = [("Int", 0), ("Bool", 0), ("List", 1), ("Pair", 2)],
    tlvLevel = 1,
    tlvWant = ["Int", "Bool"]
  },
  TypeLevelTestcase {
    tlvDesc = "generate types of size 2",
    tlvDatatypes = [("Int", 0), ("Bool", 0), ("List", 1), ("Pair", 2)],
    tlvLevel = 2,
    tlvWant = ["[Int]", "[Bool]"]
  },
  TypeLevelTestcase {
    tlvDesc = "generate types of size 3",
    tlvDatatypes = [("Int", 0), ("Bool", 0), ("List", 1), ("Pair", 2)],
    tlvLevel = 3,
    tlvWant = ["[[Int]]", "[[Bool]]", "(Int, Int)", "(Int, Bool)", "(Bool, Bool)", "(Bool, Int)"]
  },
  TypeLevelTestcase {
    tlvDesc = "generate types of size 4",
    tlvDatatypes = [("Int", 0), ("Bool", 0), ("List", 1), ("Pair", 2)],
    tlvLevel = 4,
    tlvWant = ["[[[Int]]]", "[[[Bool]]]", "[(Int, Int)]", "[(Int, Bool)]", "[(Bool, Bool)]", "[(Bool, Int)]",
               "(Int, [Int])", "(Int, [Bool])", "([Int], Int)", "([Int], Bool)", "(Bool, [Int])", "(Bool, [Bool])", "([Bool], Int)", "([Bool], Bool)"]
  }
  ]

abstractTypeTestcases :: [TypeLevelTestcase]
abstractTypeTestcases = [
  TypeLevelTestcase {
    tlvDesc = "generate abstract types of size 1",
    tlvDatatypes = [("Int", 0), ("Bool", 0), ("List", 1), ("Pair", 2)],
    tlvLevel = 1,
    tlvWant = ["Int", "Bool", "_v0"]
  },
  TypeLevelTestcase {
    tlvDesc = "generate abstract types up to size 2",
    tlvDatatypes = [("Int", 0), ("Bool", 0), ("List", 1), ("Pair", 2)],
    tlvLevel = 2,
    tlvWant = ["Int", "Bool", "[Int]", "[Bool]", "[_v0]", "_v0"]
  },
  TypeLevelTestcase {
    tlvDesc = "generate abstract types up to size 3",
    tlvDatatypes = [("Int", 0), ("Bool", 0), ("List", 1), ("Pair", 2)],
    tlvLevel = 3,
    tlvWant = ["Int", "Bool", "[Int]", "[Bool]", "[[Int]]", "[[Bool]]", "(Int, Int)", "(Int, Bool)", "(Bool, Bool)", "(Bool, Int)", "[[_v0]]", "(_v0, _v1)", "[_v0]", "_v0"]
  }
  ]

spec :: Spec
spec = do
  describe "test typesAtDepth" $ do
    mapM_ (\tc ->
      it (tlvDesc tc) $ do
        let typeBank = typesAtDepth (Set.fromList $ tlvDatatypes tc) (tlvLevel tc)
        Set.fromList (map plainShow (typeBank Map.! (tlvLevel tc))) `shouldBe` Set.fromList (tlvWant tc)
      ) typeDepthTestcases

  describe "test typesOfSize" $ do
    mapM_ (\tc ->
      it (tlvDesc tc) $ do
        let typeBank = typesOfSize (Set.fromList $ tlvDatatypes tc) (tlvLevel tc)
        Set.fromList (map plainShow (typeBank Map.! (tlvLevel tc))) `shouldBe` Set.fromList (tlvWant tc)
      ) typeSizeTestcases

  describe "test abstractTypesUptoSize" $ do
    mapM_ (\tc ->
      it (tlvDesc tc) $ do
        let typeBank = abstractTypesUptoSize (Set.fromList $ tlvDatatypes tc) (tlvLevel tc)
        Set.map plainShow typeBank `shouldBe` Set.fromList (tlvWant tc)
      ) abstractTypeTestcases