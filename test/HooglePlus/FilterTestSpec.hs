module HooglePlus.FilterTestSpec (spec) where


import Test.Hspec
import Text.Pretty.Simple
import Text.Parsec.Pos
import Control.Monad.State
import Text.Parsec.Indent
import qualified Data.Map as Map
import Data.Either

import HooglePlus.FilterTest
import Types.Filtering

modules = ["Prelude"]

runNotCrashTest modules' funcSig body =
  evalStateT (checkSolutionNotCrash (modules' ++ modules) funcSig body) emptyFilterState

itNotCrashCase :: (String, [String], String, String, Bool) -> SpecWith (Arg (IO ()))
itNotCrashCase (desc, modules, funcSig, body, expectedRetVal) =
  it desc $ do
    result <- runNotCrashTest modules funcSig body
    result `shouldBe` expectedRetVal

testNotCrashCases :: [(String, [String], String, String, Bool)]
testNotCrashCases =
  [("Succeed on poly function w/o type constrains 1", [], "a -> a", "\\x -> x", True)
  , ("Succeed on poly function w/o type constrains 2", [], "(a, b) -> b", "\\(x, y) -> y", True)
  , ("Succeed on poly function w/o type constrains 3", [], "(a, Either Int Int) -> Int", "\\(_, t) -> either id id t", True)
  , ("Succeed on infinite functions", ["GHC.List"], "a -> [a]", "\\x -> repeat x", True)
  , ("Succeed on var w/ module names", ["GHC.Int", "Data.ByteString.Lazy", "Data.ByteString.Builder"],
     "GHC.Int.Int64 -> Data.ByteString.Lazy.ByteString",
     "\\arg0 -> Data.ByteString.Builder.toLazyByteString (Data.ByteString.Builder.int64Dec arg0)", True)
  , ("Fail on invalid function 1", ["Data.Maybe"], "a -> a", "\\x -> fromJust Nothing", False)
  , ("Fail on invalid function 2", ["Data.List"], "a -> a", "\\x -> head []", False)
  , ("Fail on invalid function 3", ["Data.List"], "a -> (a, a)", "\\x -> (head [x], last [])", False)
  , ("Fail on invalid function 4", ["Data.List"], "a -> (a, a)", "\\x -> (head [x], last [])", False)
  , ("Non-deterministic function", [], "Int", "last $ repeat 5", True)
  , ("Pass w/ type class 1", [], "(Show a, Show b) => Either a b -> String", "\\x -> show x", True)]

testNotCrashHOFs :: [(String, [String], String, String, Bool)]
testNotCrashHOFs =
  [("Succeed on basic application", [], "(a -> b) -> a -> b", "\\f x -> f x", True)]


runDuplicateTest :: FilterState -> [String] -> String -> String -> IO (Bool, FilterState)
runDuplicateTest st modules' funcSig body =
  runStateT (checkDuplicates (modules ++ modules') funcSig body) st

spec :: Spec
spec =
  describe "Filter" $ do
    mapM_ itNotCrashCase testNotCrashHOFs

    mapM_ itNotCrashCase testNotCrashCases

    it "Duplicates - generate inputs and pass on base case" $ do
      (ret, FilterState {sampleResults = sample}) <- runDuplicateTest emptyFilterState [] "a -> a" "\\x -> x"
      ret `shouldBe` True
      sample `shouldNotBe` Nothing

      let Just (SampleResult inputs outputs) = sample
      length inputs `shouldBe` defaultNumChecks
      length outputs `shouldBe` 1
      
      let evalResults = head outputs
      length evalResults `shouldBe` defaultNumChecks

    it "Duplicates - reject identical solution and persist the previous state" $ do
      (ret, st) <- runDuplicateTest emptyFilterState [] "Num a => a -> a" "\\x -> x + 1"
      (ret', st') <- runDuplicateTest st [] "Num a => a -> a" "\\x -> 1 + x"
      
      ret `shouldBe` True
      ret' `shouldBe` False

      st `shouldBe` st'

    it "test - pass valid solution and add solution to the state" $ do
      (ret, st) <- runDuplicateTest emptyFilterState [] "[a] -> a" "\\x -> head x"
      (ret', st') <- runDuplicateTest st [] "[a] -> a" "\\x -> last x"
      
      ret `shouldBe` True
      ret' `shouldBe` True

      st `shouldNotBe` st'
