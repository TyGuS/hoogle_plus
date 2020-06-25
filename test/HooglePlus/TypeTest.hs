module HooglePlus.TypeTest (tests) where

import Types.Type
import Synquid.Type
import Synquid.Pretty

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

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
    , testGroup "typeSubstitute"
        [ testCase "t7 [t7 := (t9, t8), t8 := (a, b), t9 := Maybe a, t1 := (t9, t8)] == (Maybe a, (a, b))" $
            (typeSubstitute (Map.fromList
                [ ("t7", pair_ (TypeVarT "t9") (TypeVarT "t8"))
                , ("t8", pair_ (TypeVarT "a") (TypeVarT "b"))
                , ("t9", TyAppT (DatatypeT "Maybe") (TypeVarT "a"))
                , ("t1", pair_ (TypeVarT "t9") (TypeVarT "t8"))
                ]) (TypeVarT "t7")) @?= (pair_ (TyAppT (DatatypeT "Maybe") (TypeVarT "a")) (pair_ (TypeVarT "a") (TypeVarT "b")))
        ]
    ]
    where
        list_ = TyAppT (DatatypeT "List")
        pair_ = TyAppT . TyAppT (DatatypeT "Pair")
