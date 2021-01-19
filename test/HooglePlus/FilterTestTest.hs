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

runNotCrashTest :: [String] -> String -> String -> IO Bool
runNotCrashTest modules' funcSig body =
    evalStateT (checkSolutionNotCrash (modules' ++ modules) funcSig' body) emptyFilterState
        where funcSig' = (instantiateSignature . parseTypeString) funcSig

runDuplicateTest :: FilterState -> [String] -> String -> String -> IO (Bool, FilterState)
runDuplicateTest st modules' funcSig body = 
    runStateT (checkDuplicates (modules ++ modules') funcSig' body) st
        where funcSig' = (instantiateSignature . parseTypeString) funcSig

itNotCrashCase :: (String, [String], String, String, Bool) -> TestTree
itNotCrashCase (desc, modules, funcSig, body, expectedRetVal) =
    testCase ("NotCrash: " ++ desc) $ do
        result <- runNotCrashTest modules funcSig body
        result @?= expectedRetVal

itDupCase :: (String, [String], String, [String], Bool) -> TestTree
itDupCase (desc, modules, tipe, solutions, shouldPass) =
    testCase ("Duplicate: " ++ desc) $ do
        (ret, st) <- runDuplicateTest emptyFilterState modules tipe main
        -- base case: pass
        ret @?= True
        -- inductive case on new solutions
        f st tipe rest
    where
        f st _ [] = return ()
        f st tipe [s] = do
            (ret', st') <- runDuplicateTest st modules tipe s
            ret' @?= shouldPass

            let st'' = st'

            if shouldPass
                then assertBool "state not unique" (st'' /= st)
                else st'' @?= st
        f st tipe (s : solns) = do
            (ret', st') <- runDuplicateTest st modules tipe s
            f st' tipe solns

        (main : rest) = reverse solutions

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
    [ ("First order passing test", [], "Num a => a -> a", ["\\x -> x + 1", "\\x -> x + 2", "\\x -> x - 5"], True)
    , ("First order passing test 2", [], "[a] -> a", ["\\x -> head x", "\\x -> last x"], True)
    , ("First order failing test", [], "Num a => a -> a", ["\\x -> x + 1", "\\x -> 1 + x"], False)
    , ("HOF passing test", [], "Num a => (a -> a) -> (a -> a) -> a -> a", ["\\f g x -> (f . g) x", "\\f g x -> (g . f) x"], True)
    , ("HOF failing test", [], "Num a => (a -> a) -> (a -> a) -> a -> a", ["\\f g x -> (f . g) x", "\\f g x -> f (g x)"], False)
    ]

dupCheckTests :: [(String, [String], String, [String], Bool)]
dupCheckTests =
    [ ("Works for smaller examples 1", [], "[a] -> a", ["\\xs -> last $ init xs", "\\xs -> last xs"], True)
    , ("Works for bigger examples", [], "[[[a]]] -> [a]", ["\\xs -> head (concat xs)", "\\xs -> last (concat xs)"], True)
    , ("Larger approx needed 1", [], "[Int] -> (Int, Int) -> Bool", ["\\vs edge -> null (replicate (head vs) edge)", "\\vs edge -> null (replicate (last vs) edge)"], True)
    , ("Larger approx needed 2", ["Data.Maybe"], "a -> [Maybe a] -> a", ["\\x xs -> fromMaybe x (head (init xs))", "\\x xs -> fromMaybe x (last (init xs))"], True)
    , ("Larger approx needed 3", ["Data.Maybe"], "a -> [Maybe a] -> a", ["\\x xs -> fromMaybe x (head (tail xs))", "\\x xs -> fromMaybe x (last (tail xs))"], True)
    , ("Larger approx needed 4 (HOF)", [], "(a -> b) -> Int -> [a -> b]", ["\\f n -> replicate n f", "\\f n -> f : (replicate n f)"], True)
    , ("Strange Case 1", ["Data.Maybe", "Data.Either", "Data.Bool"], "a -> Maybe b -> Either a b", ["\\x mb -> maybe (Left x) Right mb", "\\x mb -> bool (Left x) (Left x) (isJust mb)"], True)
    , ("Strange Case 2", [], "Int -> [a] -> [a]", ["\\n xs -> repeat (xs !! n)", "\\n xs -> replicate n (last xs)", "\\n xs -> replicate n (head xs)", "\\n xs -> take n xs", "\\n xs -> drop n xs"], True)
    , ("Strange Case 3", ["Data.List"], "Eq a => [a] -> [a]", ["\\xs -> map head (group xs)", "\\xs -> concat (group xs)", "\\xs -> head (group xs)", "\\xs -> last (group xs)", "\\xs -> concat (group (init xs))", "\\xs -> head (group (init xs))", "\\xs -> concat (group (reverse xs))", "\\xs -> concat (group (tail xs))", "\\xs -> head (group (tail xs))", "\\xs -> init (head (group xs))", "\\xs -> concat (tail (group xs))"], True)
    ]

tests :: TestTree
tests = testGroup "Filter" $
    map itNotCrashCase testNotCrashHOFs ++
    map itNotCrashCase testNotCrashCases ++
    map itNotCrashCase testNotCrashNonTerms ++
    map itDupCase testDups ++
    map itDupCase dupCheckTests
