module HooglePlus.TypeTest (tests) where

import Types.Type
import Synquid.Type
import Synquid.Pretty

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test type operations"
    [ testGroup "toPolytype"
        [ testCase "([], a) -> forall a. a" $
            toPolytype [] (TypeVarT "a") @?= ForallT "a" (Monotype (TypeVarT "a"))
        , testCase "([a], a) -> a" $
            toPolytype ["a"] (TypeVarT "a") @?= Monotype (TypeVarT "a")
        , testCase "([b], a) -> a" $
            toPolytype ["b"] (TypeVarT "a") @?= ForallT "a" (Monotype (TypeVarT "a"))
        , testCase "([a, b], [(a, b)]) -> [(a, b)]" $
            toPolytype ["a", "b"] (list_ (pair_ (TypeVarT "a") (TypeVarT "b"))) @?= Monotype (list_ (pair_ (TypeVarT "a") (TypeVarT "b")))
        , testCase "([a], [(a, b)]) -> forall b. [(a, b)]" $
            toPolytype ["a"] (list_ (pair_ (TypeVarT "a") (TypeVarT "b"))) @?= ForallT "b" (Monotype (list_ (pair_ (TypeVarT "a") (TypeVarT "b"))))
        ]
    ]
    where
        list_ = TyAppT (DatatypeT "List")
        pair_ = TyAppT . TyAppT (DatatypeT "Pair")
