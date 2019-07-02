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
  | NoGarTyGar0 -- start from the final cover of TyGar0, no refinement
  | NoGarTyGarQ -- start from the final cover of TyGarQ, no refinement
  | NoGarTyGar0B -- start from the final cover of TyGar0B, no refinement
  | NoGarTyGarQB -- start from the final cover of TyGarQB, no refinement
  deriving(Data, Show, Eq)

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
  _disableDemand :: Bool,
  _coalesceTypes :: Bool
} deriving (Eq, Show)

makeLenses ''SearchParams

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
  _disableDemand = False,
  _coalesceTypes = True
}
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

type ExperimentName = String
expTyGarQ = "TYGAR-Q":: ExperimentName
expTyGarQNoDmd = expTyGarQ ++ "- no demand" :: ExperimentName
expTyGarQNoCoalesce = expTyGarQ ++ "- no coalescing" :: ExperimentName

expSypetClone = "Sypet-Clone" :: ExperimentName
expTyGar0 = "TYGAR-0" :: ExperimentName
expTyGarQB = "TYGAR-QB" :: ExperimentName
expTyGar0B = "TYGAR-0B" :: ExperimentName
expNoGar = "NoGar" :: ExperimentName
expNoGar0 = "NoGar0" :: ExperimentName
expNoGarTyGar0 = "NoGar with TyGar0's cover" :: ExperimentName
expNoGarTyGarQ = "NoGar with TyGarQ's cover" :: ExperimentName
expNoGarTyGar0B = "NoGar with TyGar0B's cover" :: ExperimentName
expNoGarTyGarQB = "NoGar with TyGarQB's cover" :: ExperimentName
expILP = "Integer Linear Programming" :: ExperimentName

data ExperimentCourse
  = CompareInitialAbstractCovers
  | TrackTypesAndTransitions --2019-05-06
  | CompareFinalCovers
  | CompareThresholds
  | CompareSolutions -- 2019-06-12
  | CompareEnvironments
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
