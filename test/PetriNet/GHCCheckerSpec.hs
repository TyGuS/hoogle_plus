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

-- separateFunctions :: String -> ([RType], RType)
separateFunctions query = let
    res = either (\left -> error $ show left) id $ doParseType query
    in allBaseTypes res

spec :: Spec
spec = do
    describe "mkFunctionSigStr" $ do
        it "Handles no typeclasses" $ do
            let input = "Int -> a"
            let baseTys = separateFunctions input
            let result = mkFunctionSigStr baseTys
            let expected = "(Int) -> (a)"
            result `shouldBe` expected

        it "Handles a single typeclasses" $ do
            let input = "Show a => a -> String"
            let result = mkFunctionSigStr (separateFunctions input)
            let expected = "(Show a) => (a) -> (String)"
            result `shouldBe` expected

        it "Handles multiple typeclasses on the same variable" $ do
            let input = "(Show a, Ord a) => a -> String"
            let result = mkFunctionSigStr (separateFunctions input)
            let expected = "(Show a, Ord a) => (a) -> (String)"
            result `shouldBe` expected

    describe "mkLambdaStr" $ do
        it "outputs the body of simple function" $ do
            let inputProgram = Program {
                typeOf=AnyT,
                content=PApp "arg0" [anyTy $ PSymbol "0"]
            }
            let result = mkLambdaStr ["arg0"] inputProgram
            let expected = "(\\arg0 -> arg0 0)"
            result `shouldBe` expected

        it "upgrades a typeclass used as a param" $ do
            let tcFunc = "show"
            let inputProgram = anyTy (PApp tcFunc [
                    anyTy (PSymbol "tcarg0"),
                    anyTy (PSymbol "arg0")
                    ])
            let result = mkLambdaStr ["tcarg0", "arg0"] inputProgram
            let expected = "(\\arg0 -> show arg0)"
            result `shouldBe` expected

    describe "removeTypeclassInstances" $ do
        it "removes a simple typeclass" $ do
            let input = "Text.Show.show (@@hplusTCInstance@@01Show tcarg0 tcarg1) arg0"
            let expected = "Text.Show.show arg0"
            let result = removeTypeclasses input
            result `shouldBe` expected

        it "removes a prelude typeclass usage" $ do
            let input = "Prelude.show @@hplusTCInstance@@01Show arg0"
            let expected = "Prelude.show arg0"
            let result = removeTypeclasses input
            result `shouldBe` expected

    describe "parseStrictnessSig" $ do
        it "plucks out the strictness signatures" $ do
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
            result `shouldBe` expected

        it "plucks out the strictness signatures, example 2" $ do
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
            result `shouldBe` expected

    describe "checkStrictness'" $ do
        it "should accpt a fully used lambda" $ do
            let tyclassCount = 0
            let lambdaExpr = "(\\x -> \\y -> (:) x y)"
            let typeExpr = "a -> [a] -> [a]"
            let modules = ["Prelude"]
            result <- checkStrictness' tyclassCount lambdaExpr typeExpr modules
            result `shouldBe` True

        it "should reject a partially used lambda" $ do
            let tyclassCount = 0
            let lambdaExpr = "(\\x -> \\y -> x)"
            let typeExpr = "a -> b -> a"
            let modules = ["Prelude"]
            result <- checkStrictness' tyclassCount lambdaExpr typeExpr modules
            result `shouldBe` False