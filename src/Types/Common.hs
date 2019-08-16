module Types.Common where

import Data.Map (Map)

{-
This is a place for any top-level types that will be used by many modules.
This should have no dependencies on any other files in this project. This should be
the Top Level module.
-}
type Id = String
type GroupId = String

varName = "_v"

class Freshable s where
    getNameIndices :: s -> Map Id Int
    setNameIndices :: Map Id Int -> s -> s

data TimeStatistics = TimeStatistics {
  encodingTime :: Double,
  constructionTime :: Double,
  solverTime :: Double,
  codeFormerTime :: Double,
  refineTime :: Double,
  typeCheckerTime :: Double,
  totalTime :: Double,
  iterations :: Int,
  pathLength :: Int,
  numOfTransitions :: Map Int Int,
  numOfPlaces :: Map Int Int,
  duplicateSymbols :: [(Int, Int, Int)]
} deriving(Eq)

class Timable s where
    getTimeStats :: s -> TimeStatistics
    setTimeStats :: TimeStatistics -> s -> s