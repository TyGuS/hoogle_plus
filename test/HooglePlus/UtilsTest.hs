module HooglePlus.UtilsTest (tests) where

import HooglePlus.Utils
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

doParseType tyStr = flip evalState (initialPos "goal") $ runIndentParserT parseTypeMbTypeclasses () "" tyStr

anyTy program = Program{typeOf=AnyT, content=program}

separateFunctions query = let
    res = either (\left -> error $ show left) id $ doParseType query
    in allBaseTypes res

tests :: TestTree
tests = testGroup "test utility function for demand analysis"
    [ testCase "mkFunctionSigStr: no typeclasses" $ do
        let input = "Int -> a"
        let baseTys = separateFunctions input
        let result = mkFunctionSigStr baseTys
        let expected = "Int -> a"
        result @?= expected
    , testCase "mkFunctionSigStr: a single typeclasses" $ do
        let input = "Show a => a -> String"
        let result = mkFunctionSigStr (separateFunctions input)
        let expected = "(Show a) => a -> String"
        result @?= expected
    , testCase "mkFunctionSigStr: multiple typeclasses on the same variable" $ do
        let input = "(Show a, Ord a) => a -> String"
        let result = mkFunctionSigStr (separateFunctions input)
        let expected = "(Show a, Ord a) => a -> String"
        result @?= expected
    , testCase "mkLambdaStr: outputs the body of simple function" $ do
        let inputProgram = anyTy (PApp "arg0" [anyTy $ PSymbol "0"])
        let result = mkLambdaStr ["arg0"] inputProgram
        let expected = "\\arg0 -> arg0 0"
        result @?= expected
    , testCase "mkLambdaStr: handles non-arg named arguments" $ do
        let inputProgram = anyTy (PApp "k" [anyTy $ PSymbol "0"])
        let result = mkLambdaStr ["k"] inputProgram
        let expected = "\\k -> k 0"
        result @?= expected
    , testCase "mkLambdaStr: upgrades a typeclass used as a param" $ do
        let tcFunc = "show"
        let inputProgram = anyTy (PApp tcFunc [
                anyTy (PSymbol "tcarg0"),
                anyTy (PSymbol "arg0")
                ])
        let result = mkLambdaStr ["tcarg0", "arg0"] inputProgram
        let expected = "\\arg0 -> show arg0"
        result @?= expected
    , testCase "removeTypeclassInstances: removes a simple typeclass" $ do
        let input = "Text.Show.show (@@hplusTCInstance@@01Show tcarg0 tcarg1) arg0"
        let expected = "Text.Show.show arg0"
        let result = removeTypeclasses input
        result @?= expected
    , testCase "removeTypeclassInstances: removes a prelude typeclass usage" $ do
        let input = "Prelude.show @@hplusTCInstance@@01Show arg0"
        let expected = "Prelude.show arg0"
        let result = removeTypeclasses input
        result @?= expected
    ]

