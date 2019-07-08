{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Types.Experiments where

import Types.Type
import Types.Encoder
import Types.Program
import Synquid.Program
import Synquid.Error
import Types.Common

-- import Control.Monad.List
import Data.Data
import Control.Lens hiding (index, indices)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception

{- Interface -}
data RefineStrategy =
    SypetClone -- first level abstraction, no refinement
  | TyGar0 -- start from \nu, refinement
  | TyGarQ -- start from query types, refinement
  | NoGar -- start from query types, no refinement
  | NoGar0 -- start from \nu, no refinement
  deriving(Data, Show, Eq)

data CoalesceStrategy =
    First -- First in the group set: naive
  | LeastInstantiated -- Least instantiated element in group set.
  | MostInstantiated
  deriving (Data, Show, Eq)

-- | Parameters of program exploration
data SearchParams = SearchParams {
  _maxApplicationDepth :: Int,                    -- ^ Maximum depth of application trees
  _sourcePos :: SourcePos,                -- ^ Source position of the current goal
  _explorerLogLevel :: Int,               -- ^ How verbose logging is
  _solutionCnt :: Int,
  _useHO :: Bool,
  _refineStrategy :: RefineStrategy,
  _stopRefine :: Bool,
  _threshold :: Int,
  _incrementalSolving :: Bool,
  _disableDemand :: Bool,
  _coalesceTypes :: Bool,
  _coalesceStrategy :: CoalesceStrategy
} deriving (Eq, Show)

makeLenses ''SearchParams

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
} deriving(Show, Eq)

emptyTimeStats = TimeStatistics 0 0 0 0 0 0 0 0 0 Map.empty Map.empty []

data TimeStatUpdate
  = ConstructionTime
  | EncodingTime
  | FormerTime
  | SolverTime
  | RefinementTime
  | TypeCheckTime
  | TotalSearch

-- | Parameters for template exploration
defaultSearchParams = SearchParams {
  _maxApplicationDepth = 6,
  _sourcePos = noPos,
  _explorerLogLevel = 0,
  _solutionCnt = 1,
  _useHO = False,
  _refineStrategy = TyGarQ,
  _stopRefine = False,
  _threshold = 10,
  _incrementalSolving = False,
  _disableDemand = False,
  _coalesceTypes = True,
  _coalesceStrategy = First
}

type ExperimentName = String

data ExperimentCourse
  = CompareInitialAbstractCovers
  | TrackTypesAndTransitions --2019-05-06
  | CompareFinalCovers
  | CompareThresholds
  | CompareIncremental
  | CompareSolutions -- 2019-06-12
  | CompareEnvironments
  | CoalescingStrategies
  | POPLQuality
  deriving (Show, Data, Typeable)

data Message
  = MesgClose CloseStatus
  | MesgP (RProgram, TimeStatistics) -- Program with the stats associated with generating it
  | MesgS TimeStatistics
  | MesgLog Int String String -- Log level, tag, message

data CloseStatus
  = CSNormal
  | CSNoSolution
  | CSTimeout
  | CSError SomeException
