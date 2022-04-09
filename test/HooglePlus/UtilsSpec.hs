module HooglePlus.UtilsSpec
    ( spec
    ) where

import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Text.Parsec                    ( runParser )

import           Compiler.Parser
import           HooglePlus.Utils
import           Types.Program
import           Types.Type

doParseType = runParser parseTypeMbTypeclasses () ""

-- separateFunctions :: String -> ([TypeSkeleton], TypeSkeleton)
separateFunctions :: String -> [TypeSkeleton]
separateFunctions query =
    let res = either (error . show) id $ doParseType query in allBaseTypes res

spec :: Spec
spec = do
    describe "mkFunctionSigStr" $ do
        it "Handles no typeclasses" $ do
            let input    = "Int -> a"
            let baseTys  = separateFunctions input
            let result   = mkFunctionSigStr baseTys
            let expected = "(Int) -> (a)"
            result `shouldBe` expected

        it "Handles a single typeclasses" $ do
            let input    = "Show a => a -> String"
            let result   = mkFunctionSigStr (separateFunctions input)
            let expected = "(Show a) => (a) -> (String)"
            result `shouldBe` expected

        it "Handles multiple typeclasses on the same variable" $ do
            let input    = "(Show a, Ord a) => a -> String"
            let result   = mkFunctionSigStr (separateFunctions input)
            let expected = "(Show a, Ord a) => (a) -> (String)"
            result `shouldBe` expected

    describe "mkLambdaStr" $ do
        it "outputs the body of simple function" $ do
            let inputProgram =
                    Program { typeOf = TopT, content = PApp "arg0" [varp "0"] }
            let result   = mkLambdaStr ["arg0"] inputProgram
            let expected = "(\\arg0 -> arg0 0)"
            result `shouldBe` expected

        it "handles non-arg named arguments" $ do
            let inputProgram =
                    Program { typeOf = TopT, content = PApp "k" [varp "0"] }
            let result   = mkLambdaStr ["k"] inputProgram
            let expected = "(\\k -> k 0)"
            result `shouldBe` expected

        it "upgrades a typeclass used as a param" $ do
            let tcFunc = "show"
            let
                inputProgram =
                    untyped
                        (PApp
                            tcFunc
                            [ untyped (PSymbol "tcarg0")
                            , untyped (PSymbol "arg0")
                            ]
                        )
            let result   = mkLambdaStr ["tcarg0", "arg0"] inputProgram
            let expected = "(\\arg0 -> show arg0)"
            result `shouldBe` expected

    describe "removeTypeclassInstances" $ do
        it "removes a simple typeclass" $ do
            let
                input
                    = "Text.Show.show (@@hplusTCInstance@@01Show tcarg0 tcarg1) arg0"
            let expected = "Text.Show.show arg0"
            let result   = removeTypeclasses input
            result `shouldBe` expected

        it "removes a prelude typeclass usage" $ do
            let input    = "Prelude.show @@hplusTCInstance@@01Show arg0"
            let expected = "Prelude.show arg0"
            let result   = removeTypeclasses input
            result `shouldBe` expected

