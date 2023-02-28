module HooglePlus.FilterTestSpec
  ( spec
  ) where

import           Control.Monad.State
import qualified Data.Map                      as Map
import           Test.Hspec

import           Postfilter.FilterTest
import           Types.Filtering
import           Types.Program
import Types.Type

data FilterTestCase = FilterTestCase
  { description     :: String
  , testMdls        :: [String]
  , argNames        :: [String]
  , signature       :: String
  , testProgram     :: TProgram
  , comparePrograms :: [TProgram]
  , expectedResult  :: Bool
  }

modules :: [String]
modules = ["Prelude"]

-- runNotCrashTest
--   :: MonadIO m => [String] -> [String] -> String -> TProgram -> m Bool
-- runNotCrashTest modules' argNames funcSig body = evalStateT
--   (checkSolutionNotCrash [] (modules' ++ modules) argNames funcSig body)
--   emptyFilterState

-- itNotCrashCase :: FilterTestCase -> SpecWith (Arg (IO ()))
-- itNotCrashCase (FilterTestCase desc mdls args funcSig body _ expectedRetVal) =
--   it desc $ do
--     result <- runNotCrashTest mdls args funcSig body
--     result `shouldBe` expectedRetVal

-- itDupCase :: FilterTestCase -> SpecWith (Arg (IO ()))
-- itDupCase (FilterTestCase desc mdls args tipe main rest shouldPass) =
--   it desc $ do
--     (ret, st) <- runDuplicateTest emptyFilterState mdls args tipe main

--     -- base case: pass
--     ret `shouldBe` True

--     -- inductive case on new solutions
--     mapM_ (f ret st tipe) rest
--  where
--   f ret st tipe impl = do
--     (ret', st') <- runDuplicateTest st [] args tipe impl

--     if shouldPass
--       then shouldBe ret' True >> shouldNotBe st' st
--       else shouldBe ret' False >> shouldBe st' st

--   runDuplicateTest
--     :: FilterState
--     -> Sessions
--     -> [String]
--     -> String
--     -> TProgram
--     -> IO (Bool, FilterState)
--   runDuplicateTest st sessions argNames funcSig body = runStateT
--     (checkDuplicates sessions argNames funcSig body)
--     st


-- testNotCrashCases :: [FilterTestCase]
-- testNotCrashCases =
--   [ FilterTestCase "Succeed on polymorphic function w/o type constrains 1"
--       []
--       ["x"]
--       "a -> a"
--       (funcp "x" (varp "x"))
--       []
--       True
--   , FilterTestCase "Succeed on polymorphic function w/o type constrains 2"
--       []
--       ["p"]
--       "(a, b) -> b"
--       (funcp "p" (untyped $ PApp "fst" [varp "p"])) -- "\\(x, y) -> y"
--       []
--       True
--   , FilterTestCase "Succeed on polymorphic function w/o type constrains 3"
--       []
--       ["p"]
--       "(a, Either Int Int) -> Int"
--       (funcp "p" (untyped $ PApp "either" [varp "id", varp "id", untyped $ PApp "snd" [varp "p"]])) -- "\\(_, t) -> either id id t"
--       []
--       True
--   , FilterTestCase "Succeed on infinite structures"
--       ["GHC.List"]
--       ["x"]
--       "a -> [a]"
--       (funcp "x" (untyped $ PApp "repeat" [varp "x"])) -- "\\x -> repeat x"
--       []
--       True
--   , FilterTestCase "Succeed on result with explicit module names"
--       ["GHC.List"]
--       ["arg0", "arg1"]
--       "[a] -> [b] -> [[(a,b)]]"
--       (funcp "arg0" $ funcp "arg1" $ untyped $ PApp "GHC.List.repeat" [untyped $ PApp "GHC.List.zip" [varp "arg1", varp "arg0"]]) -- "\\arg0 arg1 -> GHC.List.repeat (GHC.List.zip arg1 arg0)"
--       []
--       True
--   , FilterTestCase "Fail on invalid function 1"
--       ["Data.Maybe"]
--       ["x"]
--       "a -> a"
--       (funcp "x" $ untyped $ PApp "fromJust" [varp "Nothing"]) -- "\\x -> fromJust Nothing"
--       []
--       False
--   , FilterTestCase "Fail on invalid function 2"
--       ["Data.List"]
--       ["x"]
--       "a -> a"
--       (funcp "x" $ untyped $ PApp "head" [varp "[]"]) -- "\\x -> head []"
--       []
--       False
--   , FilterTestCase "Fail on invalid function 3"
--       ["Data.List"]
--       ["x"]
--       "a -> (a, a)"
--       (funcp "x" $ untyped $ PApp "Pair" [untyped $ PApp "head" [varp "x"], untyped $ PApp "last" [varp "[]"]]) -- "\\x -> (head [x], last [])"
--       []
--       False
--   , FilterTestCase "Fail on invalid function 4"
--       ["Data.List"]
--       ["x"]
--       "a -> (a, a)"
--       (funcp "x" $ untyped $ PApp "Pair" [untyped $ PApp "head" [varp "[]"], untyped $ PApp "last" [varp "x"]]) -- "\\x -> (head [], last [x])"
--       []
--       False
--   , FilterTestCase "Succeed on result with type class 1"
--       []
--       ["x"]
--       "(Show a, Show b) => Either a b -> String"
--       (funcp "x" $ untyped $ PApp "show" [varp "x"]) -- "\\x -> show x"
--       []
--       True
--   ]

-- -- TODO: add the test case for `foldr take xs (repeat n)`

-- testNotCrashHOFs :: [FilterTestCase]
-- testNotCrashHOFs =
--   [ FilterTestCase "Succeed on basic application"
--       []
--       ["f", "x"]
--       "(a -> b) -> a -> b"
--       (funcp "f" $ funcp "x" $ untyped $ PApp "f" [varp "x"]) -- "\\f x -> f x"
--       []
--       True
--   , FilterTestCase "Succeed on HOF with data type"
--       ["Data.Maybe"]
--       ["f", "xs"]
--       "(a -> Maybe b) -> [a] -> Maybe b"
--       (funcp "f" $ funcp "xs" $ untyped $ PApp "Data.Maybe.listToMaybe" [untyped $ PApp "Data.Maybe.mapMaybe" [varp "f", varp "xs"]]) -- "\\f xs -> Data.Maybe.listToMaybe (Data.Maybe.mapMaybe f xs)"
--       []
--       True
--   , FilterTestCase "Succeed on simple HOF"
--       ["GHC.List"]
--       ["p", "xs"]
--       "(a -> Bool) -> [a] -> Int"
--       (funcp "p" $ funcp "xs" $ untyped $ PApp "GHC.List.length" [untyped $ PApp "GHC.List.takeWhile" [varp "p", varp "xs"]]) -- "\\p xs -> GHC.List.length (GHC.List.takeWhile p xs)"
--       []
--       True
--   , FilterTestCase "Succeed on complex HOF"
--       []
--       ["h", "g", "f", "x"]
--       "(a -> b) -> (b -> c) -> (c -> d) -> a -> d"
--       (funcp "h" $ funcp "g" $ funcp "f" $ funcp "x" $ untyped $ PApp "f" [untyped $ PApp "g" [untyped $ PApp "h" [varp "x"]]]) -- "\\h g f x -> (f . g . h) x"
--       []
--       True
--   , FilterTestCase "Succeed on the strange case #138"
--       ["GHC.List"]
--       ["arg0", "arg1", "arg2"]
--       "[a] -> ([a] -> [a]) -> Int -> [a]"
--       (funcp "arg0" $ funcp "arg1" $ funcp "arg2" $ untyped $ PApp "(!!)" [untyped $ PApp "GHC.List.iterate'" [varp "arg1", varp "arg0"], varp "arg2"]) -- "\\arg0 arg1 arg2 -> (GHC.List.iterate' arg1 arg0) !! arg2"
--       []
--       True
--   ]

-- testNotCrashNonTerms :: [FilterTestCase]
-- testNotCrashNonTerms =
--   [ FilterTestCase "Succeed on infinite structures"
--       ["GHC.List"]
--       ["x"]
--       "a -> [a]"
--       (funcp "x" $ untyped $ PApp "repeat" [varp "x"]) -- "\\x -> repeat x"
--       []
--       True
--   , FilterTestCase "Fail on non-termination: basic"
--       []
--       ["x"]
--       "Int -> Int"
--       (funcp "x" $ untyped $ PApp "last" [untyped $ PApp "repeat" [varp "x"]]) -- "\\x -> last $ repeat x"
--       []
--       False
--   , FilterTestCase "Fail on non-termination: lazy evaluation 1"
--       []
--       ["x"]
--       "Int -> [Int]"
--       (funcp "x" $ untyped $ PApp "replicate" [varp "99", untyped $ PApp "length" [untyped $ PApp "repeat" [varp "x"]]]) -- "\\x -> replicate 99 (length $ repeat x)"
--       []
--       False
--   , FilterTestCase "Fail on non-termination: lazy evaluation 2"
--       []
--       ["x"]
--       "Int -> [[Int]]"
--       (funcp "x" $ untyped $ PApp "Cons" [untyped $ PApp "Cons" [untyped $ PApp "last" [untyped $ PApp "repeat" [varp "x"]], varp "Nil"], varp "Nil"]) -- "\\x -> [[last $ repeat x]]"
--       []
--       False
--   , FilterTestCase "Fail on non-termination: lazy evaluation 3"
--       []
--       ["x", "xs"]
--       "Int -> [Int] -> Int"
--       (funcp "x" $ funcp "xs" $ untyped $ PApp "(!!)" [untyped $ PApp "repeat" [varp "x"], untyped $ PApp "length" [varp "xs"]]) -- "\\x xs -> (!!) (repeat x) (length xs)"
--       []
--       False
--   ]

-- testDups :: [FilterTestCase]
-- testDups =
--   [ FilterTestCase "Dup: First order passing test"
--       []
--       ["x"]
--       "Num a => a -> a"
--       -- ["\\x -> x + 1", "\\x -> x + 2", "\\x -> x - 5"]
--       (funcp "x" $ untyped $ PApp "+" [varp "x", varp "1"]) -- "\\x -> x + 1"
--       [ (funcp "x" $ untyped $ PApp "+" [varp "x", varp "2"]) -- "\\x -> x + 2"
--       , (funcp "x" $ untyped $ PApp "-" [varp "x", varp "5"]) -- "\\x -> x - 5"
--       ]
--       True
--   , FilterTestCase "Dup: First order failing test"
--       []
--       ["x"]
--       "Num a => a -> a"
--       -- ["\\x -> x + 1", "\\x -> 1 + x"]
--       (funcp "x" $ untyped $ PApp "+" [varp "x", varp "1"]) -- "\\x -> x + 1"
--       [ (funcp "x" $ untyped $ PApp "+" [varp "1", varp "x"]) -- "\\x -> 1 + x"
--       ]
--       False
--   , FilterTestCase "Dup: First order passing test 2"
--       []
--       ["x"]
--       "[a] -> a"
--       -- ["\\x -> head x", "\\x -> last x"]
--       (funcp "x" $ untyped $ PApp "head" [varp "x"]) -- "\\x -> head x"
--       [ (funcp "x" $ untyped $ PApp "last" [varp "x"]) -- "\\x -> last x"
--       ]
--       True
--   , FilterTestCase "Dup: Higher order passing test"
--       []
--       ["f", "g", "x"]
--       "Num a => (a -> a) -> (a -> a) -> a -> a"
--       -- ["\\f g x -> (f . g) x", "\\f g x -> (g . f) x"]
--       (funcp "f" $ funcp "g" $ funcp "x" $ untyped $ PApp "f" [untyped $ PApp "g" [varp "x"]]) -- "\\f g x -> f (g x)"
--       [ (funcp "f" $ funcp "g" $ funcp "x" $ untyped $ PApp "g" [untyped $ PApp "f" [varp "x"]]) -- "\\f g x -> g (f x)"
--       ]
--       True
--   , FilterTestCase "Dup: Higher order failing test"
--       []
--       ["f", "g", "x"]
--       "Num a => (a -> a) -> (a -> a) -> a -> a"
--       -- ["\\f g x -> (f . g) x", "\\f g x -> f (g x)"]
--       (funcp "f" $ funcp "g" $ funcp "x" $ untyped $ PApp "($)" [untyped $ PApp "(.)" [varp "f", varp "g"], varp "x"]) -- "\\f g x -> (f . g) $ x"
--       [ (funcp "f" $ funcp "g" $ funcp "x" $ untyped $ PApp "f" [untyped $ PApp "g" [varp "x"]]) -- "\\f g x -> f (g x)"
--       ]
--       False
--   , FilterTestCase "Dup: Non-termination passing test"
--       ["Data.Maybe", "GHC.List"]
--       ["x", "xs"]
--       "a -> [Maybe a] -> a"
--       ((funcp "x" $ funcp "xs" $ untyped $ PApp "fromMaybe" [varp "x", untyped $ PApp "listToMaybe" [untyped $ PApp "catMaybes" [varp "xs"]]]))
--       [(funcp "x" $ funcp "xs" $ untyped $ PApp "fromMaybe" [varp "x", untyped $ PApp "last" [varp "xs"]])
--       , funcp "x" $ funcp "xs" $ untyped $ PApp "GHC.List.foldl" [varp "fromMaybe", varp "x", varp "xs"]
--       , funcp "x" $ funcp "xs" $ untyped $ PApp "fromMaybe" [varp "x", untyped $ PApp "head" [untyped $ PApp "tail" [varp "xs"]]] -- \arg1 arg0 -> fromMaybe arg0 (head (init arg1))
--       , funcp "x" $ funcp "xs" $ untyped $ PApp "fromMaybe" [varp "x", untyped $ PApp "last" [untyped $ PApp "tail" [varp "xs"]]] -- \arg1 arg0 -> fromMaybe arg0 (head (tail arg1))
--       , funcp "x" $ funcp "xs" $ untyped $ PApp "fromMaybe" [varp "x", untyped $ PApp "head" [varp "xs"]] -- \arg1 arg0 -> fromMaybe arg0 (head arg1)
--       ]
--       True
--   ]

spec :: Spec
spec = describe "Filter" $ do
--   mapM_ itNotCrashCase testNotCrashHOFs
--   mapM_ itNotCrashCase testNotCrashCases
--   mapM_ itNotCrashCase testNotCrashNonTerms

--   mapM_ itDupCase      testDups
    return ()