module HooglePlus.RefinementSpec (spec) where

import Test.Hspec

import Types.TypeChecker

spec :: Spec
spec = do
  describe "abstractApply" $ do
    it "can be parsed" $ do
      pendingWith "parseRefinement"