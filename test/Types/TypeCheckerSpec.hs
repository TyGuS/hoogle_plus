module Types.TypeCheckerSpec
  ( spec
  ) where

import           Control.Monad.State            ( evalState
                                                , runState
                                                )
import           Data.Either                    ( isLeft
                                                , isRight
                                                )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

import           Types.Environment
import           Types.Program
import           Types.Solver
import           Types.Type
import           Types.TypeChecker

runChecker :: Checker a -> (a, CheckerState)
runChecker = flip runState emptyChecker

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

  describe "bottomUpCheck" $ do
    it "`foldl ($) x xs` does not type check" $ do
      let env =
            addComponent "x" (Monotype $ TypeVarT "a")
              $ addComponent "xs" (Monotype $ listType (TypeVarT "a"))
              $ addTypeVar "a" loadEnv
      let (checkResult, _) = runChecker
            (bottomUpCheck
              Map.empty
              env
              (untyped $ PApp "GHC.List.foldl"
                              [varp "(Data.Function.$)", varp "x", varp "xs"]
              )
            )
      isLeft checkResult `shouldBe` True

    it "`foldr ($) x xs` does type check" $ do
      let env =
            addComponent "x" (Monotype $ TypeVarT "a")
              $ addComponent
                  "xs"
                  ( Monotype
                  $ listType (FunctionT "x" (TypeVarT "a") (TypeVarT "a"))
                  )
              $ addTypeVar "a" loadEnv
      let (checkResult, _) = runChecker
            (bottomUpCheck
              Map.empty
              env
              (untyped $ PApp "GHC.List.foldr"
                              [varp "(Data.Function.$)", varp "x", varp "xs"]
              )
            )
      isRight checkResult `shouldBe` True

    it "`fromLeft x (Right xs)` does type check" $ do
      let env =
            addComponent "x" (Monotype $ TypeVarT "a")
              $ addComponent "y" (Monotype $ TypeVarT "b")
              $ addTypeVar "a"
              $ addTypeVar "b" loadEnv
      let (checkResult, _) = runChecker
            (bottomUpCheck
              Map.empty
              env
              (untyped $ PApp
                "Data.Either.fromLeft"
                [varp "x", untyped $ PApp "Data.Either.Right" [varp "y"]]
              )
            )
      isRight checkResult `shouldBe` True

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
