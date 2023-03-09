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
import Types.Substitution

import Debug.Trace

runChecker :: Checker a -> (a, CheckerState)
runChecker = flip runState emptyChecker

hasType :: String -> Either TProgram TProgram -> Bool
hasType typeSig = either (const False) (\p -> plainShow (canonicalize $ typeOf p) == typeSig)

data SubtypeTestcase = SubtypeTestcase {
  subDesc :: String,
  subVars :: [Id],
  subLType :: TypeSkeleton,
  subRType :: TypeSkeleton,
  subWant :: Bool
}

subtypeTestcases :: [SubtypeTestcase]
subtypeTestcases = [
  SubtypeTestcase {
    subDesc = "TopT is subtype of TopT",
    subVars = [],
    subLType = TopT,
    subRType = TopT,
    subWant = True
  },
  SubtypeTestcase {
    subDesc = "free var is subtype of TopT",
    subVars = [],
    subLType = vart "x",
    subRType = TopT,
    subWant = True
  },
  SubtypeTestcase {
    subDesc = "bound var is subtype of TopT",
    subVars = ["x"],
    subLType = vart "x",
    subRType = TopT,
    subWant = True
  },
  SubtypeTestcase {
    subDesc = "TopT is subtype of free vars",
    subVars = [],
    subLType = TopT,
    subRType = vart "x",
    subWant = True
  },
  SubtypeTestcase {
    subDesc = "TopT is not subtype of bound vars",
    subVars = ["x"],
    subLType = TopT,
    subRType = vart "x",
    subWant = False
  },
  SubtypeTestcase {
    subDesc = "bound var is subtype of free vars",
    subVars = ["x"],
    subLType = vart "x",
    subRType = vart "y",
    subWant = True
  },
  SubtypeTestcase {
    subDesc = "[Int] is subtype of [a], but not reverse",
    subVars = [],
    subLType = listType intType,
    subRType = listType (vart "a"),
    subWant = True
  },
  SubtypeTestcase {
    subDesc = "[a] is not subtype of [Int]",
    subVars = [],
    subLType = listType (vart "a"),
    subRType = listType intType,
    subWant = False
  }
  ]

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
    buCheck = either (const False) (\p -> plainShow (canonicalize $ typeOf p) == "t0 -> [t0] -> t0")
  },
  BottomUpTestcase {
    buDesc = "fromMaybe x (listToMaybe xs) checks fail",
    buArgs = [("x", vart "a"), ("xs", listType (DatatypeT "Maybe" [vart "a"]))],
    buVars = ["a"],
    buProgram = untyped $ PApp "Data.Maybe.fromMaybe" [varp "x", untyped $ PApp "Data.Maybe.listToMaybe" [varp "xs"]],
    buCheck = either (\p -> plainShow p == "Data.Maybe.fromMaybe x (Data.Maybe.listToMaybe xs)" && typeOf p == BotT) (const False)
  },
  BottomUpTestcase {
    buDesc = "\\arg0 arg1 -> filter (fromMaybe arg0) (concat arg1) infers correct type",
    buArgs = [],
    buVars = [],
    buProgram = untyped $ PFun "x0" $ untyped $ PFun "x1" $ untyped $ PApp "GHC.List.filter" [untyped $ PApp "Data.Maybe.fromMaybe" [varp "x0"], untyped $ PApp "GHC.List.concat" [varp "x1"]],
    buCheck = either (const False) (\p -> plainShow (canonicalize $ typeOf p) == "Bool -> [[Maybe Bool]] -> [Maybe Bool]")
  },
  BottomUpTestcase {
    buDesc = "\\arg0 -> maybe 0 (fromMaybe arg0) infers correct type",
    buArgs = [],
    buVars = [],
    buProgram = untyped $ PFun "x0" $ untyped $ PApp "Data.Maybe.maybe" [varp "0", untyped $ PApp "Data.Maybe.fromMaybe" [varp "x0"]],
    buCheck = either (const False) (\p -> plainShow (canonicalize $ typeOf p) == "Int -> Maybe (Maybe Int) -> Int")
  },
  BottomUpTestcase {
    buDesc = "\\arg0 arg1 -> [] : (arg0 : arg1) infers correct type",
    buArgs = [],
    buVars = [],
    buProgram = untyped $ PFun "x0" $ untyped $ PFun "x1" $ untyped $ PApp "Cons" [varp "Nil", untyped $ PApp "Cons" [varp "x0", varp "x1"]],
    buCheck = hasType "[t0] -> [[t0]] -> [[t0]]"
  }
  ]

data TypeConstraintTestcase = TypeConstraintTestcase {
  tcDesc :: String,
  tcVars :: [Id],
  tcConstraint :: UnifConstraint,
  tcWant :: Maybe TypeSubstitution
}

typeConstraintTestcases :: [TypeConstraintTestcase]
typeConstraintTestcases = [
  TypeConstraintTestcase {
    tcDesc = "`b -> a -> b` does not unify with `(a -> b) -> a -> b`",
    tcVars = [],
    tcConstraint = UnifiesWith (FunctionT "y" (TypeVarT "b") $ FunctionT "x" (TypeVarT "a") (TypeVarT "b"))
                               (FunctionT "f" (FunctionT "x" (TypeVarT "c") (TypeVarT "d")) (FunctionT "y" (TypeVarT "c") (TypeVarT "d"))),
    tcWant = Nothing
  },
  TypeConstraintTestcase {
    tcDesc = "`b -> a -> b` is not subtype of `(a -> b) -> a -> b`",
    tcVars = [],
    tcConstraint = SubtypeOf (FunctionT "y" (TypeVarT "b") $ FunctionT "x" (TypeVarT "a") (TypeVarT "b"))
                             (FunctionT "f" (FunctionT "x" (TypeVarT "c") (TypeVarT "d")) (FunctionT "y" (TypeVarT "c") (TypeVarT "d"))),
    tcWant = Nothing
  },
  TypeConstraintTestcase {
    tcDesc = "`List a` unifies with `List b`",
    tcVars = [],
    tcConstraint = UnifiesWith (listType (vart "a")) (listType (vart "b")),
    tcWant = Just (Map.fromList [("a",TypeVarT "b")])
  },
  TypeConstraintTestcase {
    tcDesc = "occurs check failed the unification",
    tcVars = [],
    tcConstraint = UnifiesWith (listType (vart "a")) (listType (pairType (vart "a") (vart "b"))),
    tcWant = Nothing
  }
  ]

spec :: Spec
spec = do
  describe "test isSubtypeOf" $ do
    mapM_ (\tc ->
      it (subDesc tc) (isSubtypeOf (subVars tc) (subLType tc) (subRType tc) `shouldBe` subWant tc)
      ) subtypeTestcases

  describe "test solveTypeConstraint" $
    mapM_ (\tc ->
      it (tcDesc tc) $
        solveTypeConstraint (tcVars tc) Map.empty (tcConstraint tc) `shouldBe` tcWant tc
      ) typeConstraintTestcases

  describe "bottomUpCheck" $ do
    mapM_ (\tc -> it (buDesc tc) $ do
      let env = addComponent "'a'" (Monotype charType) $ addComponent "0" (Monotype intType) $ foldr addTypeVar loadEnv (buVars tc)
      let env' = foldr (\(x, t) -> addComponent x (Monotype t)) env (buArgs tc)
      let checkResult = runTypeChecker Map.empty env' emptyChecker (buProgram tc)
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
