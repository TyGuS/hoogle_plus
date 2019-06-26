module PetriNet.GHCCheckerSpec (spec) where

import PetriNet.GHCChecker
import Synquid.Parser
import Synquid.Pretty () -- Instances
import Types.Type
import Types.Program
import Synquid.Type
import Synquid.Logic
import Database.Util

import Test.Hspec
import Text.Pretty.Simple
import Text.Parsec.Pos
import Control.Monad.State
import Text.Parsec.Indent
import qualified Data.Map as Map
import Data.Either


doParseType tyStr = flip evalState (initialPos "goal") $ runIndentParserT parseTypeMbTypeclasses () "" tyStr

anyTy program = Program{typeOf=AnyT, content=program}

separateFunctions :: String -> ([RType], RType)
separateFunctions query = let
    res = either (\left -> error $ show left) id $ doParseType query
    args = allArgTypes res
    retType = lastType res
    in (args, retType)

spec :: Spec
spec = do
    describe "GHCChecker to haskell type writer" $ do
        it "Handles no typeclasses" $ do
            let input = "Int -> a"
            let (args, retType) = separateFunctions input
            let result = mkFunctionSigStr args retType
            let expected = "(Int) -> (a)"
            result `shouldBe` expected

        it "Handles a single typeclasses" $ do
            let input = "Show a => a -> String"
            let (args, retType) = separateFunctions input
            let result = mkFunctionSigStr args retType
            let expected = "(Show a) => (a) -> (String)"
            result `shouldBe` expected

    describe "GHCChecker to haskell lambda writer" $ do
        it "outputs the body of simple function" $ do
            let inputProgram = Program {
                typeOf=AnyT,
                content=PApp "arg0" [anyTy $ PSymbol "0"]
            }
            let result = mkLambdaStr ["arg0"] inputProgram
            let expected = "(\\arg0 -> arg0 0)"
            result `shouldBe` expected

        it "upgrades a typeclass" $ do
            let tcFunc = typeclassPrefix ++ "Show"
            let inputProgram = anyTy (PApp tcFunc [
                    anyTy (PSymbol "tcarg0"),
                    anyTy (PSymbol "arg0")
                    ])
            let result = mkLambdaStr ["tcarg0", "arg0"] inputProgram
            let expected = "(\\arg0 -> Show arg0)"
            result `shouldBe` expected