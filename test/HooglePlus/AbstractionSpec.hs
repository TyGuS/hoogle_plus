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

typeLevelTestcases :: [TypeLevelTestcase]
typeLevelTestcases = [
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

spec :: Spec
spec = do
  describe "test typesAtLevel" $ do
    mapM_ (\tc ->
      it (tlvDesc tc) $ do
        let typeBank = typesAtLevel (Set.fromList $ tlvDatatypes tc) (tlvLevel tc)
        Set.fromList (map plainShow (typeBank Map.! (tlvLevel tc))) `shouldBe` Set.fromList (tlvWant tc)
      ) typeLevelTestcases