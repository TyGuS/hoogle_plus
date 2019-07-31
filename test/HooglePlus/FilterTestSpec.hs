module HooglePlus.FilterTestSpec (spec) where


import Test.Hspec
import Text.Pretty.Simple
import Text.Parsec.Pos
import Control.Monad.State
import Text.Parsec.Indent
import qualified Data.Map as Map
import Data.Either

import HooglePlus.FilterTest

importList = ["Prelude", "Data.List", "Data.Maybe"]

spec :: Spec
spec =
  describe "Filter" $ do

  it "Return Correct Type" $ do
    result <- getTypeString importList "\\x -> [x]"
    case result of
      Left err -> error "failed"
      Right str -> str `shouldBe` "a -> [a]"
  
  it "Succeed on poly function w/o type constrains" $ do
    result <- runNoExceptionTest "\\x -> x"
    result `shouldBe` True

  it "Pass function w/ different retval - singleList" $ do
    result <- runGhcChecks "\\x -> [x]"
    result `shouldBe` True

  it "Pass function w/ different retval - headLast" $ do
    result <- runGhcChecks "\\x -> (head x, last x)"
    result `shouldBe` True

  it "Fail on invalid fcns - head []" $ do
    result <- runGhcChecks "\\x -> (head x, last [])"
    result `shouldBe` False

  it "Fail on invalid fcns - just nothing" $ do
    result <- runGhcChecks "\\x -> fromJust Nothing"
    result `shouldBe` False

  it "Fail on const fcns" $ do
    result <- runGhcChecks "\\x -> \\y -> const x y"
    result `shouldBe` False




-- quickCheckWith stdArgs{replay = Just (mkQCGen 4, 0)}
-- stack test --test-arguments "--match /HooglePlus.FilterTest/Filter"