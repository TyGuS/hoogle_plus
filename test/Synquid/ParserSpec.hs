module Synquid.ParserSpec (spec) where

import Synquid.Parser
import Synquid.Pretty () -- Instances
import Types.Type
import Database.Utils

import Test.Hspec
import Text.Pretty.Simple
import Text.Parsec.Pos
import Control.Monad.State
import Text.Parsec.Indent
import qualified Data.Map.Strict as Map


doParseType tyStr = flip evalState (initialPos "goal") $ runIndentParserT parseTypeMbTypeclasses () "" tyStr

spec :: Spec
spec = do
    describe "parseType' on typeclasses" $ do
        it "works with no typeclasses" $ do
            let input = "Int -> b"
            let result = doParseType input
            let expected = Right (FunctionT "arg0" (DatatypeT "Int") (TypeVarT "b"))
            result `shouldBe` expected

        it "handles a single typeclass constraint" $ do
            let input = "Show a => a"
            let typeclassName = tyclassPrefix ++ "Show"
            let result = doParseType input
            let expected = Right (
                    FunctionT "tcarg0" (TyAppT (DatatypeT typeclassName) (TypeVarT "a")) (
                        TypeVarT "a"
                        ))
            result `shouldBe` expected

        it "handles multiple typeclass constraints" $ do
            let input = "(Show a, Ord b) => a -> b"
            let showable = tyclassPrefix ++ "Show"
            let ord = tyclassPrefix ++ "Ord"
            let result = doParseType input
            let expected = Right (
                    FunctionT "tcarg0" (TyAppT (DatatypeT showable) (TypeVarT "a")) $
                    FunctionT "tcarg1" (TyAppT (DatatypeT ord) (TypeVarT "b")) $
                    FunctionT "arg0" (TypeVarT "a") (TypeVarT "b"))
            result `shouldBe` expected
