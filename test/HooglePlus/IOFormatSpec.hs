module HooglePlus.IOFormatSpec
    ( spec
    ) where

import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

import HooglePlus.IOFormat
import Database.Dataset

spec :: Spec
spec = do
  describe "searchExamples" $ do
    it "search example with no type class" $ do
      let inStr = "{\"exampleExisting\": [{\"inputs\": [\"aaaa\",\"bbb\",\"a\",\"b\"], \"output\": \"abab\"}, {\"inputs\": [\"[[1,1,1],[2,2],[3]]\"], \"output\": \"[1,2,3]\"}], \"exampleProgram\": \"\\\\xs -> Data.Maybe.mapMaybe Data.Maybe.listToMaybe xs\", \"exampleQuery\": \"xs:[[a]] -> [a]\"}"
      res <- searchExamples includedModules inStr 2
      res `shouldBe` ()

    it "search example with one type class" $ do
      let inStr = "{\"exampleExisting\": [{\"inputs\": [\"aaaabbbab\"], \"output\": \"abab\"}, {\"inputs\": [\"[1,1,1,2,2,3]\"], \"output\": \"[1,2,3]\"}], \"exampleProgram\": \"\\\\xs -> Data.Maybe.mapMaybe Data.Maybe.listToMaybe (Data.List.group xs)\", \"exampleQuery\": \"Eq a => xs:[a] -> [a]\"}"
      res <- searchExamples includedModules inStr 1
      res `shouldBe` ()

