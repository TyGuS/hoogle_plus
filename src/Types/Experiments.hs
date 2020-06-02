{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Types.Experiments where

import Types.Type
import Types.Program
import Types.IOFormat
import Synquid.Program
import Synquid.Error
import Types.Common
import Types.Filtering
import Types.Generate

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

data SolverName =
      Z3SMT
    | Z3SAT
    | CBC
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
    _coalesceStrategy :: CoalesceStrategy,
    _disableRelevancy :: Bool,
    _disableCopy :: Bool,
    _disableBlack :: Bool,
    _disableFilter :: Bool,
    _solver :: SolverName
} deriving (Eq, Show)

makeLenses ''SearchParams

data TimeStatistics = TimeStatistics {
    _encodingTime :: Double,
    _constructionTime :: Double,
    _solverTime :: Double,
    _codeFormerTime :: Double,
    _refineTime :: Double,
    _typeCheckerTime :: Double,
    _totalTime :: Double,
    _iterations :: Int,
    _pathLength :: Int,
    _numOfTransitions :: Map Int Int,
    _numOfPlaces :: Map Int Int,
    _duplicateSymbols :: [(Int, Int, Int)]
} deriving(Show, Eq)

emptyTimeStats = TimeStatistics 0 0 0 0 0 0 0 0 0 Map.empty Map.empty []

makeLenses ''TimeStatistics

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
    _useHO = True,
    _refineStrategy = TyGarQ,
    _stopRefine = True,
    _threshold = 10,
    _incrementalSolving = False,
    _disableDemand = False,
    _coalesceTypes = True,
    _coalesceStrategy = First,
    _disableRelevancy = False,
    _disableCopy = False,
    _disableBlack = False,
    _disableFilter = True,
    _solver = Z3SMT
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
    | CompareCopyTransitions
    | CoalescingStrategies
    | POPLQuality
    | POPLSpeed
    deriving (Show, Data, Typeable)

data Message
    = MesgClose CloseStatus
    | MesgP (QueryOutput, TimeStatistics, FilterState) -- Program with the stats associated with generating it
    | MesgS TimeStatistics
    | MesgLog Int String String -- Log level, tag, message

data CloseStatus
    = CSNormal
    | CSNoSolution
    | CSTimeout
    | CSError SomeException

-- | Parameters of the synthesis
data SynquidParams = SynquidParams {
    envPath :: String, -- ^ Path to the environment file
    jsonPath :: String
}

defaultSynquidParams = SynquidParams {
    Types.Experiments.envPath = defaultEnvPath,
    jsonPath = defaultJsonPath
}
