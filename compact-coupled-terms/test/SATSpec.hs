{-# LANGUAGE OverloadedStrings #-}

module SATSpec ( spec ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Test.Hspec

import SAT

-----------------------------------------------------------------

smallFormula :: CNF
smallFormula = And [ Or [PosLit "x1", PosLit "x2"]
                   , Or [NegLit "x1", NegLit "x2"]
                   ]

--------

spec :: Spec
spec = do
  describe "SAT test inputs" $ do
    it "solves a 2-var, 2-clause problem" $
      allSolutions smallFormula `shouldBe` (HashSet.fromList [ HashMap.fromList [("x1", True), ("x2", False)]
                                                             , HashMap.fromList [("x1", False), ("x2", True)]
                                                             ])