module Main where

import Criterion.Main

import Data.ECTA
import Data.ECTA.Paths

import TestData

-----------------------------------------------------------------------


main = do
  defaultMain [
                bgroup "pathable" [
                  bench "getPath" $ whnf nodeCount $ getPath (path [2,0,2]) aBigNode
                ]
              ]