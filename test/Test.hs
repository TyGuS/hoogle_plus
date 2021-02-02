module Main where

import qualified HooglePlus.FilterTestTest as HF
import qualified HooglePlus.GHCCheckerTest as HG
import qualified HooglePlus.UtilsTest as HU

import Test.Tasty

main = defaultMain $ testGroup "Tests"
    [ HF.tests
    , HG.tests
    , HU.tests
    ]
