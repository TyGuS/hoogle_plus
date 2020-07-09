module HooglePlus.FilterTestTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple
import Text.Parsec.Pos
import Control.Monad.State
import Text.Parsec.Indent
import qualified Data.Map.Strict as Map
import Data.Either

import HooglePlus.FilterTest
import Types.Filtering

modules = ["Prelude"]

runNotCrashTest modules' funcSig body =
    evalStateT (checkSolutionNotCrash (modules' ++ modules) funcSig body) emptyFilterState

itNotCrashCase :: (String, [String], String, String, Bool) -> TestTree
itNotCrashCase (desc, modules, funcSig, body, expectedRetVal) =
    testCase desc $ do
        result <- runNotCrashTest modules funcSig body
        result @?= expectedRetVal

itDupCase :: (String, [String], String, [String], Bool) -> TestTree
itDupCase (desc, modules, tipe, main : rest, shouldPass) =
    testCase desc $ do
        (ret, st) <- runDuplicateTest emptyFilterState modules tipe main
        -- base case: pass
        ret @?= True
        -- inductive case on new solutions
        mapM_ (f ret st tipe) rest
    where
        f ret st tipe impl = do
            (ret', st') <- runDuplicateTest st [] tipe impl

            if shouldPass then (ret' @?= True) >> assertBool "state not unique" (st' /= st)
                          else (ret' @?= False) >> (st' @?= st)

        runDuplicateTest :: FilterState -> [String] -> String -> String -> IO (Bool, FilterState)
        runDuplicateTest st modules' funcSig body = 
            runStateT (checkDuplicates (modules ++ modules') funcSig body) st

testNotCrashCases :: [(String, [String], String, String, Bool)]
testNotCrashCases =
    [ ("Succeed on polymorphic function w/o type constrains 1", [], "a -> a", "\\x -> x", True)
    , ("Succeed on polymorphic function w/o type constrains 2", [], "(a, b) -> b", "\\(x, y) -> y", True)
    , ("Succeed on polymorphic function w/o type constrains 3", [], "(a, Either Int Int) -> Int", "\\(_, t) -> either id id t", True)
    , ("Succeed on infinite structures", ["GHC.List"], "a -> [a]", "\\x -> repeat x", True)
    , ("Succeed on result with explicit module names", ["GHC.List"], "[a] -> [b] -> [[(a,b)]]", "\\arg0 arg1 -> GHC.List.repeat (GHC.List.zip arg1 arg0)", True)
    , ("Fail on invalid function 1", ["Data.Maybe"], "a -> a", "\\x -> fromJust Nothing", False)
    , ("Fail on invalid function 2", ["Data.List"], "a -> a", "\\x -> head []", False)
    , ("Fail on invalid function 3", ["Data.List"], "a -> (a, a)", "\\x -> (head [x], last [])", False)
    , ("Fail on invalid function 4", ["Data.List"], "a -> (a, a)", "\\x -> (head [], last [x])", False)
    , ("Succeed on result with type class 1", [], "(Show a, Show b) => Either a b -> String", "\\x -> show x", True)
    ]

testNotCrashHOFs :: [(String, [String], String, String, Bool)]
testNotCrashHOFs =
    [ ("Succeed on basic application", [], "(a -> b) -> a -> b", "\\f x -> f x", True)
    , ("Succeed on HOF with data type", ["Data.Maybe"], "(a -> Maybe b) -> [a] -> Maybe b", "\\f xs -> Data.Maybe.listToMaybe (Data.Maybe.mapMaybe f xs)", True)
    , ("Succeed on simple HOF", ["GHC.List"], "(a -> Bool) -> [a] -> Int", "\\p xs -> GHC.List.length (GHC.List.takeWhile p xs)", True)
    , ("Succeed on complex HOF", [], "(a -> b) -> (b -> c) -> (c -> d) -> a -> d", "\\h g f x -> (f . g . h) x", True)
    , ("Succeed on the strange case #138", ["GHC.List"], "[a] -> ([a] -> [a]) -> Int -> [a]", "\\arg0 arg1 arg2 -> (GHC.List.iterate' arg1 arg0) !! arg2", True)
    ]

testNotCrashNonTerms :: [(String, [String], String, String, Bool)]
testNotCrashNonTerms =
    [ ("Succeed on infinite structures", ["GHC.List"], "a -> [a]", "\\x -> repeat x", True)
    , ("Fail on non-termination: basic", [], "Int -> Int", "\\x -> last $ repeat x", False)
    , ("Fail on non-termination: lazy evaluation 1", [], "Int -> [Int]", "\\x -> replicate 99 (length $ repeat x)", False)
    , ("Fail on non-termination: lazy evaluation 2", [], "Int -> [[Int]]", "\\x -> [[last $ repeat x]]", False)
    ]

testDups :: [(String, [String], String, [String], Bool)]
testDups =
    [ ("Dup: First order passing test", [], "Num a => a -> a", ["\\x -> x + 1", "\\x -> x + 2", "\\x -> x - 5"], True)
    , ("Dup: First order failing test", [], "Num a => a -> a", ["\\x -> x + 1", "\\x -> 1 + x"], False)
    , ("Dup: First order passing test 2", [], "[a] -> a", ["\\x -> head x", "\\x -> last x"], True)
    , ("Dup: Higher order passing test", [], "Num a => (a -> a) -> (a -> a) -> a -> a", ["\\f g x -> (f . g) x", "\\f g x -> (g . f) x"], True)
    , ("Dup: Higher order failing test", [], "Num a => (a -> a) -> (a -> a) -> a -> a", ["\\f g x -> (f . g) x", "\\f g x -> f (g x)"], False)
    ]

tests :: TestTree
tests = testGroup "Filter" $
    map itNotCrashCase testNotCrashHOFs ++
    map itNotCrashCase testNotCrashCases ++
    map itNotCrashCase testNotCrashNonTerms ++
    map itDupCase testDups
