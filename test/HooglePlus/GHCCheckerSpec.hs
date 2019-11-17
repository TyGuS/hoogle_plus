module HooglePlus.GHCCheckerSpec (spec) where

import HooglePlus.GHCChecker
import Synquid.Parser
import Synquid.Pretty () -- Instances
import Types.Type
import Types.Program
import Synquid.Type
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

separateFunctions query = let
    res = either (\left -> error $ show left) id $ doParseType query
    in allBaseTypes res

spec :: Spec
spec = do
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