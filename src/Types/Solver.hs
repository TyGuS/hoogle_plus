module Types.Solver where

import           Control.Lens                   ( makeLenses )
import           Control.Monad.Logic            ( LogicT )
import           Control.Monad.State            ( StateT )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Text.PrettyPrint.ANSI.Leijen   ( string )
import           Text.Printf                    ( printf )

import           Database.Environment
import           Types.Common
import           Types.Encoder
import           Types.Experiments       hiding ( PetriNet )
import           Types.Filtering
import           Types.Program
import           Types.Type
import           Types.TypeChecker


data SearchState = SearchState
  { _currentSolutions :: [RProgram] -- type checked solutions
  , _currentLoc       :: Int -- current solution depth
  , _currentSigs      :: Map Id TypeSkeleton -- current type signature groups
  , _activeSigs       :: Set Id
  , _functionMap      :: HashMap Id EncodedFunction
  }
  deriving Eq

emptySearchState :: SearchState
emptySearchState = SearchState { _currentSolutions = []
                               , _currentLoc       = 0
                               , _currentSigs      = Map.empty
                               , _activeSigs       = Set.empty
                               , _functionMap      = HashMap.empty
                               }

makeLenses ''SearchState

data StatisticState = StatisticState
  { _instanceCounts :: HashMap Id Int -- Number of instantiations for a real-name, used in selecting representative
  , _useCount       :: Map Id Int
  }
  deriving Eq

emptyStatistic :: StatisticState
emptyStatistic =
  StatisticState { _instanceCounts = HashMap.empty, _useCount = Map.empty }

makeLenses ''StatisticState

data CheckError = CheckError
  { errorProgram :: TProgram
  , desiredType  :: TypeSkeleton
  }
  deriving Eq

data RefineState = RefineState
  { _abstractionCover :: AbstractCover
  , _instanceMapping  :: HashMap (Id, [TypeSkeleton]) (Id, TypeSkeleton)
  , _targetType       :: TypeSkeleton
  , _sourceTypes      :: [TypeSkeleton]
  , _splitTypes       :: Set TypeSkeleton
  , _toRemove         :: [Id]
  , _passOneOrMore    :: Bool
  , -- ^ whether all the possible filling of a sketch all type checks
    _lastError        :: CheckError -- ^ the last type checking error, used for refinement
  }
  deriving Eq

emptyRefineState :: RefineState
emptyRefineState = RefineState { _abstractionCover = Map.empty
                               , _instanceMapping  = HashMap.empty
                               , _targetType       = TypeVarT varName
                               , _sourceTypes      = []
                               , _splitTypes       = Set.empty
                               , _toRemove         = []
                               , _passOneOrMore    = False
                               , _lastError        = undefined
                               }

makeLenses ''RefineState

data SolverState = SolverState
  { _searchParams :: SearchParams
  , _refineState  :: RefineState
  , _statistics   :: StatisticState
  , _searchState  :: SearchState
  , _groupState   :: GroupResult
  , _encoder      :: EncodeState
  , _typeChecker  :: CheckerState
  , _filterState  :: FilterState
  }
  deriving Eq

emptySolverState :: SolverState
emptySolverState = SolverState { _searchParams = defaultSearchParams
                               , _refineState  = emptyRefineState
                               , _statistics   = emptyStatistic
                               , _searchState  = emptySearchState
                               , _groupState   = emptyGroup
                               , _encoder      = emptyEncodeState
                               , _typeChecker  = emptyChecker
                               , _filterState  = emptyFilterState
                               }

makeLenses ''SolverState

type PNSolver m = StateT SolverState m
type BackTrack m = LogicT (PNSolver m)

data SearchResult = NotFound
                  | Found (TProgram, AssociativeExamples)
                  | MoreRefine (TProgram, TypeSkeleton)
                  deriving(Eq)
