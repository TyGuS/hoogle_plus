module Types.Experiments where

import           Control.Lens                   ( makeLenses )
import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

import           Types.Generate

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
data SearchParams = SearchParams
  { _maxApplicationDepth :: Int -- ^ Maximum depth of application trees
  , _logLevel            :: Int -- ^ How verbose logging is
  , _solutionCnt         :: Int
  , _useHO               :: Bool
  , _refineStrategy      :: RefineStrategy
  , _stopRefine          :: Bool
  , _threshold           :: Int
  , _incrementalSolving  :: Bool
  , _disableDemand       :: Bool
  , _coalesceTypes       :: Bool
  , _coalesceStrategy    :: CoalesceStrategy
  , _disableRelevancy    :: Bool
  , _disableCopy         :: Bool
  , _disableBlack        :: Bool
  , _disableFilter       :: Bool
  }
  deriving (Eq, Show)

makeLenses ''SearchParams

data TimeStatUpdate
  = ConstructionTime
  | EncodingTime
  | FormerTime
  | SolverTime
  | RefinementTime
  | TypeCheckTime
  | TotalSearch

-- | Parameters for template exploration
defaultSearchParams :: SearchParams
defaultSearchParams = SearchParams { _maxApplicationDepth = 6
                                   , _logLevel            = 0
                                   , _solutionCnt         = 1
                                   , _useHO               = True
                                   , _refineStrategy      = TyGarQ
                                   , _stopRefine          = True
                                   , _threshold           = 10
                                   , _incrementalSolving  = False
                                   , _disableDemand       = False
                                   , _coalesceTypes       = True
                                   , _coalesceStrategy    = First
                                   , _disableRelevancy    = False
                                   , _disableCopy         = False
                                   , _disableBlack        = False
                                   , _disableFilter       = True
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

-- | Parameters of the synthesis
data SynquidParams = SynquidParams
  { envPath  :: String
  , -- ^ Path to the environment file
    jsonPath :: String
  }

defaultSynquidParams :: SynquidParams
defaultSynquidParams = SynquidParams
  { Types.Experiments.envPath = defaultEnvPath
  , jsonPath                  = defaultJsonPath
  }
