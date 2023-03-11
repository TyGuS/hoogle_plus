module Types.AntiunifierSpec
  ( spec
  ) where

import qualified Data.Map as Map

import Test.Hspec (Spec, describe, it, shouldBe)

import Types.Pretty
import Types.Type
import Types.TypeChecker
import Types.Substitution
import Types.Antiunifier

data AuTestcase a = AuTestcase {
  auDesc :: String,
  auLhs :: a,
  auRhs :: a,
  auWant :: a
}

auTypeTestcases :: [AuTestcase TypeSkeleton]
auTypeTestcases = [
  AuTestcase {
    auDesc = "antiunify([Int], Int) = * ",
    auLhs = listType intType,
    auRhs = intType,
    auWant = vart "t0"
  },
  AuTestcase {
    auDesc = "antiunify([Int], [Bool]) = [*] ",
    auLhs = listType intType,
    auRhs = listType boolType,
    auWant = listType (vart "t0")
  },
  AuTestcase {
    auDesc = "antiunify((Int, Int), (Bool, Bool)) = (*, *) ",
    auLhs = pairType intType intType,
    auRhs = pairType boolType boolType,
    auWant = pairType (vart "t0") (vart "t0")
  },
  AuTestcase {
    auDesc = "antiunify((Int, Char), (Bool, Bool)) = (*0, *1) ",
    auLhs = pairType intType intType,
    auRhs = pairType boolType charType,
    auWant = pairType (vart "t0") (vart "t1")
  },
  AuTestcase {
    auDesc = "antiunify((Int, Char), (Bool, Char)) = (*, Char) ",
    auLhs = pairType intType charType,
    auRhs = pairType boolType charType,
    auWant = pairType (vart "t0") charType
  },
  AuTestcase {
    auDesc = "antiunify(?x, (Bool, Char)) = (Bool, Char) ",
    auLhs = exists "x",
    auRhs = pairType boolType charType,
    auWant = pairType boolType charType
  },
  AuTestcase {
    auDesc = "antiunify([?x], [Bool]) = [Bool]",
    auLhs = listType (exists "x"),
    auRhs = listType boolType,
    auWant = listType boolType
  }
  ]

auSubstTestcases :: [AuTestcase TypeSubstitution]
auSubstTestcases = [
  AuTestcase {
    auDesc = "antiunify(*0 |-> Int || *0 |-> Int)",
    auLhs = Map.fromList [("*0", intType)],
    auRhs = Map.fromList [("*0", intType)],
    auWant = Map.fromList [("*0", intType)]
  },
  AuTestcase {
    auDesc = "antiunify(*0 |-> [Int] || *0 |-> [Bool])",
    auLhs = Map.fromList [("*0", listType intType)],
    auRhs = Map.fromList [("*0", listType boolType)],
    auWant = Map.fromList [("*0", listType (vart "*0"))]
  },
  AuTestcase {
    auDesc = "antiunify(*0 |-> [Int], *1 |-> (Int, Bool), *2 |-> [Bool] || *0 |-> [Bool], *1 |-> (Int, Int), *2 |-> Maybe Bool)",
    auLhs = Map.fromList [("*0", listType intType), ("*1", pairType intType boolType), ("*2", listType boolType)],
    auRhs = Map.fromList [("*0", listType boolType), ("*1", pairType intType intType), ("*2", DatatypeT "Maybe" [boolType])],
    auWant = Map.fromList [("*0", listType (vart "*0")), ("*1", pairType intType (vart "*1")), ("*2", vart "*2")]
  }
  ]

spec :: Spec
spec = do
  describe "test antiunification of types" $
    mapM_ (\tc ->
      it (auDesc tc) $ do
        t <- runAntiunifier $ antiunify (auLhs tc) (auRhs tc)
        plainShow (canonicalize t) `shouldBe` plainShow (auWant tc)
      ) auTypeTestcases

  describe "test antiunification of substitutions" $
    mapM_ (\tc ->
      it (auDesc tc) $ do
        subst <- runAntiunifier $ antiunify (auLhs tc) (auRhs tc)
        subst `shouldBe` auWant tc
      ) auSubstTestcases

