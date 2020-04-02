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

itDupCase :: (String, [String], String, [String], Bool) -> SpecWith (Arg (IO ()))
itDupCase (desc, modules, tipe, (main: rest), shouldPass) =
  it desc $ do
    (ret, st) <- runDuplicateTest emptyFilterState modules tipe main

    -- base case: pass
    ret `shouldBe` True

    -- inductive case on new solutions
    mapM_ (f ret st tipe) rest
  where
    f ret st tipe impl = do
      (ret', st') <- runDuplicateTest st [] tipe impl

      if shouldPass then
        shouldBe ret' True >> shouldNotBe st' st
        
      else
        shouldBe ret' False >> shouldBe st' st

    runDuplicateTest :: FilterState -> [String] -> String -> String -> IO (Bool, FilterState)
    runDuplicateTest st modules' funcSig body = 
      runStateT (checkDuplicates (modules ++ modules') funcSig body) st


testNotCrashCases :: [(String, [String], String, String, Bool)]
testNotCrashCases =
  [("Succeed on polymorphic function w/o type constrains 1", [], "a -> a", "\\x -> x", True)
  , ("Succeed on polymorphic function w/o type constrains 2", [], "(a, b) -> b", "\\(x, y) -> y", True)
  , ("Succeed on polymorphic function w/o type constrains 3", [], "(a, Either Int Int) -> Int", "\\(_, t) -> either id id t", True)
  , ("Succeed on infinite structures", ["GHC.List"], "a -> [a]", "\\x -> repeat x", True)
  , ("Succeed on result with explicit module names", ["GHC.List"],
     "[a] -> [b] -> [[(a,b)]]",
     "\\arg0 arg1 -> GHC.List.repeat (GHC.List.zip arg1 arg0)", True)
  , ("Fail on invalid function 1", ["Data.Maybe"], "a -> a", "\\x -> fromJust Nothing", False)
  , ("Fail on invalid function 2", ["Data.List"], "a -> a", "\\x -> head []", False)
  , ("Fail on invalid function 3", ["Data.List"], "a -> (a, a)", "\\x -> (head [x], last [])", False)
  , ("Fail on invalid function 4", ["Data.List"], "a -> (a, a)", "\\x -> (head [], last [x])", False)
  , ("Non-deterministic function 1", [], "Int", "last $ repeat 5", False)
  , ("Non-deterministic function 2", [], "a -> Int", "\\x -> last $ repeat x", False)
  , ("Succeed on result with type class 1", [], "(Show a, Show b) => Either a b -> String", "\\x -> show x", True)]

testNotCrashHOFs :: [(String, [String], String, String, Bool)]
testNotCrashHOFs =
  [("Succeed on basic application", [], "(a -> b) -> a -> b", "\\f x -> f x", True)
  , ("Succeed on HOF with data type", ["Data.Maybe"], "(a -> Maybe b) -> [a] -> Maybe b", "\\f xs -> Data.Maybe.listToMaybe (Data.Maybe.mapMaybe f xs)", True)
  , ("Succeed on simple HOF", ["GHC.List"], "(a -> Bool) -> [a] -> Int", "\\p xs -> GHC.List.length (GHC.List.takeWhile p xs)", True)
  , ("Succeed on complex HOF", [], "(a -> b) -> (b -> c) -> (c -> d) -> a -> d", "\\h g f x -> (f . g . h) x", True)]

testDups :: [(String, [String], String, [String], Bool)]
testDups =
  [("Dup: First order passing test", [], "Num a => a -> a", ["\\x -> x + 1", "\\x -> x + 2", "\\x -> x - 5"], True)
  , ("Dup: First order failing test", [], "Num a => a -> a", ["\\x -> x + 1", "\\x -> 1 + x"], False)
  , ("Dup: First order passing test 2", [], "[a] -> a", ["\\x -> head x", "\\x -> last x"], True)
  , ("Dup: Higher order passing test", [], "Num a => (a -> a) -> (a -> a) -> a -> a", ["\\f g x -> (f . g) x", "\\f g x -> (g . f) x"], True)
  , ("Dup: Higher order failing test", [], "Num a => (a -> a) -> (a -> a) -> a -> a", ["\\f g x -> (f . g) x", "\\f g x -> f (g x)"], False)
  ]

spec :: Spec
spec =
  describe "Filter" $ do
    mapM_ itNotCrashCase testNotCrashHOFs
    mapM_ itNotCrashCase testNotCrashCases
    mapM_ itDupCase testDups

    -- it "Duplicates - generate inputs and pass on base case" $ do
    --   (ret, FilterState {sampleResults = sample}) <- runDuplicateTest emptyFilterState [] "a -> a" "\\x -> x"
    --   ret `shouldBe` True
    --   sample `shouldNotBe` Nothing

    --   let Just (SampleResult inputs outputs ) = sample
    --   length inputs `shouldBe` defaultNumChecks
    --   length outputs `shouldBe` 1
      
    --   let evalResults = head outputs
    --   length evalResults `shouldBe` defaultNumChecks
