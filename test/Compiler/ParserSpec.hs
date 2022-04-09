module Compiler.ParserSpec
    ( spec
    ) where

import qualified Data.Map                      as Map
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Text.Parsec                    ( runParser )

import           Compiler.Parser
import           Types.Type
import           Utility.Utils

doParseType = runParser parseTypeMbTypeclasses () ""

spec :: Spec
spec = do
    describe "parseType' on typeclasses" $ do
        it "works with no typeclasses" $ do
            let input    = "Int -> b"
            let result   = doParseType input
            let expected = Right (FunctionT "arg0" intType (TypeVarT "b"))
            result `shouldBe` expected

        it "handles a single typeclass constraint" $ do
            let input         = "Show a => a"
            let typeclassName = appendSuffix tyclassPrefix "Show"
            let result        = doParseType input
            let expected = Right
                    (FunctionT "tcarg0"
                               (DatatypeT typeclassName [TypeVarT "a"])
                               (TypeVarT "a")
                    )
            result `shouldBe` expected

        it "handles multiple typeclass constraints" $ do
            let input    = "(Show a, Ord b) => a -> b"
            let showable = appendSuffix tyclassPrefix "Show"
            let ord      = appendSuffix tyclassPrefix "Ord"
            let result   = doParseType input
            let expected = Right
                    ( FunctionT "tcarg0" (DatatypeT showable [TypeVarT "a"])
                    $ FunctionT "tcarg1" (DatatypeT ord [TypeVarT "b"])
                    $ FunctionT "arg0" (TypeVarT "a") (TypeVarT "b")
                    )
            result `shouldBe` expected
