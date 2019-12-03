module HooglePlus.FilterTestSpec (spec) where


import Test.Hspec
import Text.Pretty.Simple
import Text.Parsec.Pos
import Control.Monad.State
import Text.Parsec.Indent
import qualified Data.Map.Strict as Map
import Data.Either

import HooglePlus.FilterTest

modules = ["Prelude"]
runTest modules' = runChecks' (modules' ++ modules)

itCase :: (String, [String], String, String, Bool) -> SpecWith (Arg (IO ()))
itCase (desc, modules, funcSig, body, expectedRetVal) =
  it desc $ do
    result <- runTest modules funcSig body
    result `shouldBe` expectedRetVal

testCases :: [(String, [String], String, String, Bool)]
testCases = 
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
  , ("Fail on invalid function 5", ["Data.Maybe"], "Either a b -> Maybe a", "\\x -> fromJust Nothing", False)
  , ("Pass w/ type class 1", [], "(Show a, Show b) => Either a b -> String", "\\x -> show x", True)]

spec :: Spec
spec =
  describe "Filter" $ mapM_ itCase testCases