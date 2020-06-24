module HooglePlus.TypeCheckerTest (tests) where

import HooglePlus.TypeChecker
import Types.Environment
import Types.TypeChecker
import Types.Type

import Test.Tasty
import Test.Tasty.HUnit
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map

tests :: TestTree
tests = testGroup "Test TypeChecker"
    [ testGroup "solveTypeConstraint"
        [ testCase "bottom ~ bottom" $
            checkTypeSolver BottomT BottomT True Map.empty
        , testCase "a ~ a" $
            checkTypeSolver (TypeVarT "a") (TypeVarT "a") True Map.empty
        , testCase "a /~ b" $
            checkTypeSolver (TypeVarT "a") (TypeVarT "b") False Map.empty
        , testCase "a ~ T" $
            checkTypeSolver (TypeVarT "a") (TypeVarT "T") True (Map.singleton "T" (TypeVarT "a"))
        , testCase "T1 ~ T2" $
            checkTypeSolver (TypeVarT "T1") (TypeVarT "T2") True (Map.singleton "T1" (TypeVarT "T2"))
        , testCase "[a] ~ [T]" $
            checkTypeSolver (list_ (TypeVarT "a")) (list_ (TypeVarT "T")) True (Map.singleton "T" (TypeVarT "a"))
        , testCase "Either a b ~ Either T1 T2" $
            checkTypeSolver (either_ (TypeVarT "a") (TypeVarT "b")) (either_ (TypeVarT "T1") (TypeVarT "T2"))
                            True (Map.fromList [("T1", TypeVarT "a"), ("T2", TypeVarT "b")])
        , testCase "[a] /~ a" $
            checkTypeSolver (list_ (TypeVarT "a")) (TypeVarT "a") False Map.empty
        , testCase "[T1] /~ T1" $
            checkTypeSolver (list_ (TypeVarT "T1")) (TypeVarT "T1") False Map.empty
        , testCase "Either a T1 ~ Either T1 T2" $
            checkTypeSolver (either_ (TypeVarT "a") (TypeVarT "T1")) (either_ (TypeVarT "T1") (TypeVarT "T2"))
                            True (Map.fromList [("T1", TypeVarT "a"), ("T2", TypeVarT "a")])
        , testCase "T1 -> T2 -> Either T1 T2 ~ T4 -> T3 -> Either T3 T4" $
            checkTypeSolver (FunctionT "" (TypeVarT "T1") (FunctionT "" (TypeVarT "T2") (either_ (TypeVarT "T1") (TypeVarT "T2"))))
                            (FunctionT "" (TypeVarT "T4") (FunctionT "" (TypeVarT "T3") (either_ (TypeVarT "T3") (TypeVarT "T4"))))
                            True (Map.fromList [("T1", TypeVarT "T4"), ("T2", TypeVarT "T3"), ("T4", TypeVarT "T3")])
        , testCase "(a -> b) -> [a] -> [b] ~ T1 -> T2 -> T2" $
            checkTypeSolver (FunctionT "" (TyFunT (TypeVarT "a") (TypeVarT "b")) (FunctionT "" (list_ (TypeVarT "a")) (list_ (TypeVarT "b"))))
                            (FunctionT "" (TypeVarT "T1") (FunctionT "" (TypeVarT "T2") (TypeVarT "T2")))
                            False Map.empty
        , testCase "(a -> b) -> [a] -> [b] ~ (T1 -> T1) -> T1 -> T1" $
            checkTypeSolver (FunctionT "" (TyFunT (TypeVarT "a") (TypeVarT "b")) (FunctionT "" (list_ (TypeVarT "a")) (list_ (TypeVarT "b"))))
                            (FunctionT "" (TyFunT (TypeVarT "T1") (TypeVarT "T1")) (FunctionT "" (TypeVarT "T1") (TypeVarT "T1")))
                            False Map.empty
        , testCase "[Either a b] -> Either a [b] ~ [Either T1 b] -> Either T2 T3" $
            checkTypeSolver (FunctionT "" (list_ (either_ (TypeVarT "a") (TypeVarT "b"))) (either_ (TypeVarT "a") (list_ (TypeVarT "b"))))
                            (FunctionT "" (list_ (either_ (TypeVarT "T1") (TypeVarT "b"))) (either_ (TypeVarT "T2") (TypeVarT "T3")))
                            True (Map.fromList [("T1", TypeVarT "a"), ("T2", TypeVarT "a"), ("T3", list_ (TypeVarT "b"))])
        ]
    ]
    where
        checkTypeSolver t1 t2 b m = do
            let st = execState (solveTypeConstraint env t1 t2) emptyChecker
            (st ^. isChecked) @?= b
            when (st ^. isChecked) ((st ^. typeAssignment) @?= m)
        env = emptyEnv { _boundTypeVars = ["a", "b", "c"] }
        list_ = TyAppT (DatatypeT "List")
        either_ = TyAppT . TyAppT (DatatypeT "Either")
