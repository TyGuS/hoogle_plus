module Synquid.ParserSpec (spec) where

import Synquid.Parser
import Synquid.Pretty () -- Instances
import Types.Type
import Synquid.Logic
import Database.Util

import Test.Hspec
import Text.Pretty.Simple
import Text.Parsec.Pos
import Control.Monad.State
import Text.Parsec.Indent
import qualified Data.Map as Map


doParseType tyStr = flip evalState (initialPos "goal") $ runIndentParserT parseTypeMbTypeclasses () "" tyStr
mkTyVar str = ScalarT (TypeVarT (Map.empty) str) ftrue

spec :: Spec
spec = do
    describe "parseType' on typeclasses" $ do
        it "works with no typeclasses" $ do
            let input = "Int -> b"
            let result = doParseType input
            let expected = Right (
                    FunctionT "arg0" (ScalarT (DatatypeT "Int" [] []) ftrue) (mkTyVar "b")
                    )
            result `shouldBe` expected

        it "handles a single typeclass constraint" $ do
            let input = "Show a => a"
            let typeclassName = tyclassPrefix ++ "Show"
            let result = doParseType input
            let expected = Right (
                    FunctionT "tcarg0" (ScalarT (DatatypeT typeclassName [mkTyVar "a"] []) ftrue) (
                        mkTyVar "a"
                        ))
            result `shouldBe` expected

        it "handles multiple typeclass constraints" $ do
            let input = "(Show a, Ord b) => a -> b"
            let showable = tyclassPrefix ++ "Show"
            let ord = tyclassPrefix ++ "Ord"
            let result = doParseType input
            let expected = Right (
                    FunctionT "tcarg0" (ScalarT (DatatypeT showable [mkTyVar "a"] []) ftrue) $
                    FunctionT "tcarg1" (ScalarT (DatatypeT ord [mkTyVar "b"] []) ftrue) $
                    FunctionT "arg0" (mkTyVar "a") (mkTyVar "b"))
            result `shouldBe` expected
