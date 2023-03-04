module Utility.HashJoinSpec ( spec) where

import Data.List ( nub, sort )

import Test.Hspec
import Test.QuickCheck

import Utility.HashJoin

-----------------------------------------------------------------

spec :: Spec
spec = do
  describe "hash utilities" $ do
    it "nubById is same as nub" $
      property $ \(xs :: [Int]) -> sort (nub xs) == sort (nubById id xs)