module HooglePlus.GHCCheckerTest (tests) where

import HooglePlus.GHCChecker
import Synquid.Parser
import Synquid.Pretty () -- Instances
import Types.Type
import Types.Program
import Synquid.Type
import Database.Utils

import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple
import Text.Parsec.Pos
import Control.Monad.State
import Text.Parsec.Indent
import qualified Data.Map.Strict as Map
import Data.Either

tests :: TestTree
tests = testGroup "GHCChecker"
    [ testCase "parseStrictnessSig: plucks out the strictness signatures" $ do
        let sig = unlines [ "foo :: forall a b. a -> a -> (a -> b) -> a"
                            , "[LclIdX,"
                            , "Arity=3,"
                            , "Str=<S,1*U><L,A><L,A>,"
                            , "Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,"
                            , "    WorkFree=True, Expandable=True,"
                            , "    Guidance=ALWAYS_IF(arity=3,unsat_ok=True,boring_ok=True)}]"
                            , "foo"
                            , "  = \\ (@ a)"
                            , "      (@ b)"
                            , "      (arg0 [Dmd=<S,1*U>] :: a)"
                            , "      _ [Occ=Dead, Dmd=<L,A>]"
                            , "      _ [Occ=Dead, Dmd=<L,A>] ->"
                            , "      arg0"
                            ]
        let expected = "<S,1*U><L,A><L,A>"
        let result = parseStrictnessSig sig
        result @?= expected
    , testCase "parseStrictnessSig: plucks out the strictness signatures, example 2" $ do
        let sig = unlines [ "foo :: forall a. a -> [a] -> [a]"
                            , "[LclIdX,"
                            , "Arity=2,"
                            , "Str=<L,U><L,U>m2,"
                            , "Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,"
                            , "    WorkFree=True, Expandable=True,"
                            , "    Guidance=ALWAYS_IF(arity=3,unsat_ok=True,boring_ok=True)}]"
                            , "foo"
                            ]
        let expected = "<L,U><L,U>"
        let result = parseStrictnessSig sig
        result @?= expected
    , testCase "checkStrictness': accept a fully used lambda" $ do
        let tyclassCount = 0
        let lambdaExpr = "(\\x -> \\y -> (:) x y)"
        let typeExpr = "a -> [a] -> [a]"
        let modules = ["Prelude"]
        result <- checkStrictness' tyclassCount lambdaExpr typeExpr modules
        result @?= True

    , testCase "checkStrictness': reject a partially used lambda" $ do
        let tyclassCount = 0
        let lambdaExpr = "(\\x -> \\y -> x)"
        let typeExpr = "a -> b -> a"
        let modules = ["Prelude"]
        result <- checkStrictness' tyclassCount lambdaExpr typeExpr modules
        result @?= False
    ]
