module HooglePlus.FilterTestSpec (spec) where


import Test.Hspec
import Text.Pretty.Simple
import Text.Parsec.Pos
import Control.Monad.State
import Text.Parsec.Indent
import qualified Data.Map as Map
import Data.Either

import HooglePlus.FilterTest

modules = ["Prelude"]

runTest modules' = runChecks' (modules' ++ modules)

spec :: Spec
spec =
  describe "Filter" $ do

  it "Succeed on poly function w/o type constrains 1" $ do
    result <- runTest [] "a -> a" "\\x -> x"
    result `shouldBe` True
  
  it "Succeed on poly function w/o type constrains 2" $ do
    result <- runTest [] "(a, b) -> b" "\\(x, y) -> y"
    result `shouldBe` True

  it "Succeed on poly function w/o type constrains 3" $ do
    result <- runTest [] "(a, Either Int Int) -> Int" "\\(_, t) -> either id id t"
    result `shouldBe` True
  
  it "Succeed on infinite functions" $ do
    result <- runTest ["GHC.List"] "a -> [a]" "\\x -> repeat x"
    result `shouldBe` True

  it "Succeed on var w/ module names" $ do
    result <- runTest ["GHC.Int", "Data.ByteString.Lazy", "Data.ByteString.Builder"] "GHC.Int.Int64 -> Data.ByteString.Lazy.ByteString" "\\arg0 -> Data.ByteString.Builder.toLazyByteString (Data.ByteString.Builder.int64Dec arg0)"
    result `shouldBe` True

  it "Fail on invalid function 1" $ do
    result <- runTest ["Data.Maybe"] "a -> a" "\\x -> fromJust Nothing"
    result `shouldBe` False

  it "Fail on invalid function 2" $ do
    result <- runTest ["Data.List"] "a -> a" "\\x -> head []"
    result `shouldBe` False

  it "Fail on invalid function 3" $ do
    result <- runTest ["Data.List"] "a -> (a, a)" "\\x -> (head [x], last [])"
    result `shouldBe` False

  it "Fail on invalid function 4" $ do
    result <- runTest ["Data.List"] "a -> (a, a)" "\\x -> (head [x], last [])"
    result `shouldBe` False

  it "Fail on invalid function 6" $ do
    result <- runTest ["Data.Maybe"] "Either a b -> Maybe a" "\\x -> fromJust Nothing"
    result `shouldBe` False