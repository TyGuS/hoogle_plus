module Types.PetriNet.EquivalenceSpec (spec) where

import Types.PetriNet
import PetriNet.PNBuilder

import Test.Hspec
import Text.Pretty.Simple

spec :: Spec
spec = do
  describe "Petri Net Transition Equivalence" $ do
    it "Should find duplicate transitions of the same type" $ do
      let pn = buildPetriNet testFuncs []
      pPrint pn
      let dupes = undefined pn
      dupes `shouldBe` (2, 3)

testFuncs = [
  FunctionCode "head" [] ["List A"] ["A"],
  FunctionCode "tail" [] ["List A"] ["A"],
  FunctionCode "append" [] ["List A", "List A"] ["List A"]
  ]
