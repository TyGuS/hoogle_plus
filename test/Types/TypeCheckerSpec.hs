module Types.TypeCheckerSpec
  ( spec
  ) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalState, runState)
import Data.Either (isLeft, isRight, fromRight, either)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Hspec (Spec, describe, it, shouldBe)

import Types.Common
import Types.Environment
import Types.Pretty
import Types.Program hiding (canonicalize)
import Types.Solver
import Types.Type
import Types.TypeChecker

import Debug.Trace

runChecker :: Checker a -> (a, CheckerState)
runChecker = flip runState emptyChecker

data BottomUpTestcase = BottomUpTestcase {
  buDesc :: String,
  buArgs :: [(Id, TypeSkeleton)],
  buVars :: [Id],
  buProgram :: TProgram,
  buCheck :: Either TProgram TProgram -> Bool
}

bottomUpTestcases :: [BottomUpTestcase]
bottomUpTestcases = [
  BottomUpTestcase {
    buDesc = "`foldl ($) x xs` does not pass type checking",
    buArgs = [("x", TypeVarT "a"), ("xs", listType (TypeVarT "a"))],
    buVars = ["a"],
    buProgram = untyped $ PApp "GHC.List.foldl'" [varp "(Data.Function.$)", varp "x", varp "xs"],
    buCheck = isLeft
  },
  BottomUpTestcase {
    buDesc = "`foldr ($) x xs` passes type checking",
    buArgs = [("x", TypeVarT "a"), ("xs", listType (FunctionT "x" (TypeVarT "a") (TypeVarT "a")))],
    buVars = ["a"],
    buProgram = untyped $ PApp "GHC.List.foldr" [varp "(Data.Function.$)", varp "x", varp "xs"],
    buCheck = isRight
  },
  BottomUpTestcase {
    buDesc = "`fromLeft x (Right xs)` passes type checking",
    buArgs = [("x", TypeVarT "a"), ("y", TypeVarT "b")],
    buVars = ["a", "b"],
    buProgram = untyped $ PApp "Data.Either.fromLeft" [varp "x", untyped $ PApp "Data.Either.Right" [varp "y"]],
    buCheck = isRight
  },
  BottomUpTestcase {
    buDesc = "\\x -> x (True) get inferred type Bool -> T0",
    buArgs = [],
    buVars = [],
    buProgram = untyped $ PFun "x" (untyped $ PApp "x" [varp "Data.Bool.True"]),
    buCheck = either (const False) (\p -> plainShow (canonicalize $ typeOf p) == "(Bool -> t0) -> t0")
  },
  BottomUpTestcase {
    buDesc = "\\x0 x1 -> fromMaybe x0 (listToMaybe x1) gets inferred type t0 -> [t0] -> t0",
    buArgs = [],
    buVars = [],
    buProgram = untyped $ PFun "x0" $ untyped $ PFun "x1" (untyped $ PApp "Data.Maybe.fromMaybe" [varp "x0", untyped $ PApp "Data.Maybe.listToMaybe" [varp "x1"]]),
    buCheck = either (const False) (\p -> traceShow (typeOf p) $ plainShow (canonicalize $ typeOf p) == "t0 -> [t0] -> t0")
  }
  ]

spec :: Spec
spec = do
  describe "isSubtypeOf" $ do
    it "TopT is subtype of TopT" $ isSubtypeOf [] TopT TopT `shouldBe` True

    it "free var is subtype of TopT"
      $          isSubtypeOf [] (TypeVarT "x") TopT
      `shouldBe` True

    it "bound var is subtype of TopT"
      $          isSubtypeOf ["x"] (TypeVarT "x") TopT
      `shouldBe` True

    it "TopT is subtype of free vars"
      $          isSubtypeOf [] TopT (TypeVarT "x")
      `shouldBe` True

    it "TopT is not subtype of bound vars"
      $          isSubtypeOf ["x"] TopT (TypeVarT "x")
      `shouldBe` False

    it "bound var is subtype of free vars"
      $          isSubtypeOf ["x"] (TypeVarT "x") (TypeVarT "y")
      `shouldBe` True

    it "[Int] is subtype of [a], but not reverse" $ do
      isSubtypeOf [] (listType intType) (listType (TypeVarT "a"))
        `shouldBe` True
      isSubtypeOf [] (listType (TypeVarT "a")) (listType intType)
        `shouldBe` False

  describe "solveTypeConstraint" $ do
    it "`b -> a -> b` does not unify with `(a -> b) -> a -> b`" $ do
      let
        t1 = FunctionT "y" (TypeVarT "b")
          $ FunctionT "x" (TypeVarT "a") (TypeVarT "b")
      let t2 = FunctionT "f"
                         (FunctionT "x" (TypeVarT "c") (TypeVarT "d"))
                         (FunctionT "y" (TypeVarT "c") (TypeVarT "d"))
      solveTypeConstraint [] Map.empty (UnifiesWith t1 t2) `shouldBe` Nothing
      solveTypeConstraint [] Map.empty (SubtypeOf t2 t1) `shouldBe` Nothing

    it "`List a` unifies with `List b`" $ do
      let t1 = listType (TypeVarT "a")
      let t2 = listType (TypeVarT "b")
      solveTypeConstraint [] Map.empty (UnifiesWith t1 t2) `shouldBe` Just (Map.fromList [("a",TypeVarT "b")])

  describe "bottomUpCheck" $ do
    mapM_ (\tc -> it (buDesc tc) $ do
      let env = foldr addTypeVar loadEnv (buVars tc)
      let env' = foldr (\(x, t) -> addComponent x (Monotype t)) env (buArgs tc)
      let (checkResult, _) = runChecker $ runExceptT $ bottomUpCheck Map.empty env' (buProgram tc)
      (buCheck tc) checkResult `shouldBe` True
      ) bottomUpTestcases

  describe "abstractApply" $ do
    it "fromRight [a] a ==> _|_" $ do
      let cover = Map.fromList
            [ (TopT, Set.fromList [TypeVarT "a", listType (TypeVarT "A1")])
            , ( listType (TypeVarT "A1")
              , Set.fromList [listType $ DatatypeT "Maybe" [TypeVarT "a"]]
              )
            ]
      let fromRightTyp = FunctionT
            "x"
            (TypeVarT "A2")
            (FunctionT "e"
                       (DatatypeT "Either" [TypeVarT "A3", TypeVarT "A2"])
                       (TypeVarT "A2")
            )
      let argTyps = [listType $ DatatypeT "Maybe" [TypeVarT "a"], TypeVarT "a"]
      evalState (abstractApply ["a"] cover fromRightTyp argTyps)
                emptySolverState
        `shouldBe` BotT
      evalState
          (do
            t1 <- abstractStep ["a"] cover fromRightTyp (head argTyps)
            abstractStep ["a"] cover t1 (last argTyps)
          )
          emptySolverState
        `shouldBe` BotT
