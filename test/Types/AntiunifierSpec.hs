module Types.AntiunifierSpec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Types.Pretty
import Types.Type
import Types.TypeChecker
import Types.Antiunifier

data AuTypeTestcase = AuTypeTestcase {
  auTypeDesc :: String,
  auTypeLhs :: TypeSkeleton,
  auTypeRhs :: TypeSkeleton,
  auTypeWant :: String
}

auTypeTestcases :: [AuTypeTestcase]
auTypeTestcases = [
  AuTypeTestcase {
    auTypeDesc = "antiunify([Int], Int) = * ",
    auTypeLhs = listType intType,
    auTypeRhs = intType,
    auTypeWant = "t0"
  },
  AuTypeTestcase {
    auTypeDesc = "antiunify([Int], [Bool]) = [*] ",
    auTypeLhs = listType intType,
    auTypeRhs = listType boolType,
    auTypeWant = "[t0]"
  },
  AuTypeTestcase {
    auTypeDesc = "antiunify((Int, Int), (Bool, Bool)) = (*, *) ",
    auTypeLhs = pairType intType intType,
    auTypeRhs = pairType boolType boolType,
    auTypeWant = "(t0, t0)"
  },
  AuTypeTestcase {
    auTypeDesc = "antiunify((Int, Char), (Bool, Bool)) = (*0, *1) ",
    auTypeLhs = pairType intType intType,
    auTypeRhs = pairType boolType charType,
    auTypeWant = "(t0, t1)"
  },
  AuTypeTestcase {
    auTypeDesc = "antiunify((Int, Char), (Bool, Char)) = (*, Char) ",
    auTypeLhs = pairType intType charType,
    auTypeRhs = pairType boolType charType,
    auTypeWant = "(t0, Char)"
  },
  AuTypeTestcase {
    auTypeDesc = "antiunify(?x, (Bool, Char)) = (Bool, Char) ",
    auTypeLhs = exists "x",
    auTypeRhs = pairType boolType charType,
    auTypeWant = "(Bool, Char)"
  },
  AuTypeTestcase {
    auTypeDesc = "antiunify([?x], [Bool]) = [Bool]",
    auTypeLhs = listType (exists "x"),
    auTypeRhs = listType boolType,
    auTypeWant = "[Bool]"
  }
  ]

spec :: Spec
spec = do
  describe "test antiunification of types" $
    mapM_ (\tc ->
      it (auTypeDesc tc) $ do
        t <- runAntiunifier $ antiunify (auTypeLhs tc) (auTypeRhs tc)
        plainShow (canonicalize t) `shouldBe` auTypeWant tc
      ) auTypeTestcases