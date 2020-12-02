module Synquid.ParserTest (tests) where

import Synquid.Parser
import Synquid.Pretty () -- Instances
import Types.Type
import Database.Utils

import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple
import Text.Parsec.Pos
import Control.Monad.State
import Text.Parsec.Indent
import qualified Data.Map.Strict as Map


doParseType tyStr = flip evalState (initialPos "goal") $ runIndentParserT parseTypeMbTypeclasses () "" tyStr

tests :: TestTree
tests = testGroup "Demand Analysis" 
    [ testCase "no typeclasses" $ do
        let result = doParseType "Int -> b"
        let expected = Right (FunctionT "arg0" (DatatypeT "Int") (TypeVarT "b"))
        result @?= expected
    , testCase "single typeclass constraint" $ do
        let result = doParseType "Show a => a"
        let expected = Right (FunctionT "tcarg0" (TyAppT (DatatypeT (tyclassPrefix ++ "Show")) (TypeVarT "a")) (TypeVarT "a"))
        result @?= expected
    , testCase "multiple typeclass constraints" $ do
        let result = doParseType "(Show a, Ord b) => a -> b"
        let showable = tyclassPrefix ++ "Show"
        let ord = tyclassPrefix ++ "Ord"
        let expected = Right (
                FunctionT "tcarg0" (TyAppT (DatatypeT showable) (TypeVarT "a")) $
                FunctionT "tcarg1" (TyAppT (DatatypeT ord) (TypeVarT "b")) $
                FunctionT "arg0" (TypeVarT "a") (TypeVarT "b"))
        result @?= expected
    ]
