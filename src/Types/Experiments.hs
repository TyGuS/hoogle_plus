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

-- | Choices for the type of path search
data PathStrategy =
  MaxSAT -- ^ Use SMT solver to find a path
  | PetriNet -- ^ Use PetriNet and SyPet
  | PNSMT -- ^ Use PetriNet and SMT solver
  deriving (Eq, Show, Data)

data RefineStrategy =
    NoRefine
  | AbstractRefinement
  | Combination
  | QueryRefinement
  deriving(Data, Show, Eq)

-- | Parameters of program exploration
data SearchParams = SearchParams {
  _eGuessDepth :: Int,                    -- ^ Maximum depth of application trees
  _sourcePos :: SourcePos,                -- ^ Source position of the current goal
  _explorerLogLevel :: Int,               -- ^ How verbose logging is
  _solutionCnt :: Int,
  _encoderType :: EncoderType,
  _pathSearch :: PathStrategy,
  _useHO :: Bool,
  _useRefine :: RefineStrategy
}

makeLenses ''SearchParams

data TimeStatistics = TimeStatistics {
  encodingTime :: Double,
  constructionTime :: Double,
  solverTime :: Double,
  codeFormerTime :: Double,
  refineTime :: Double,
  typeCheckerTime :: Double,
  otherTime :: Double,
  totalTime :: Double,
  iterations :: Int,
  pathLength :: Int,
  numOfTransitions :: Map Int Int,
  numOfPlaces :: Map Int Int
} deriving(Show, Eq)

emptyTimeStats = TimeStatistics 0 0 0 0 0 0 0 0 0 0 Map.empty Map.empty

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
  _eGuessDepth = 6,
  _sourcePos = noPos,
  _explorerLogLevel = 0,
  _solutionCnt = 1,
  _pathSearch = PetriNet,
  _useHO = False,
  _encoderType = Normal,
  _useRefine = QueryRefinement
}

type ExperimentName = String
expQueryRefinement = "Query Refinement":: ExperimentName
expQueryRefinementHOF = "Query Refinement - HOF" :: ExperimentName
expBaseline = "Baseline" :: ExperimentName
expZeroCoverStart = "Zero Cover Start" :: ExperimentName


currentExperiment = TrackTypesAndTransitions

data ExperimentCourse
  = CompareInitialAbstractCovers
  | TrackTypesAndTransitions --2019-05-06

data Message
  = MesgClose CloseStatus
  | MesgP (RProgram, TimeStatistics) -- Program with the stats associated with generating it
  | MesgD TimeStatistics

data CloseStatus
  = CSNormal
  | CSError SomeException
