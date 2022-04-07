module Types.Solver where

import           Control.Lens                   ( makeLenses
                                                , set
                                                , view
                                                )
import           Control.Monad.Logic            ( LogicT )
import           Control.Monad.State            ( StateT
                                                , gets
                                                , modify
                                                , MonadIO
                                                )
import           Control.Monad.Trans            ( lift )
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
import           Types.Environment
import           Types.Experiments       hiding ( PetriNet )
import           Types.Filtering
import           Types.Fresh
import           Types.Log
import           Types.Program
import           Types.Type
import           Types.TypeChecker


data SearchState = SearchState
  { _currentSolutions :: [TProgram] -- type checked solutions
  , _currentLoc       :: Int -- current solution depth
  , _currentSigs      :: Map Id TypeSkeleton -- current type signature groups
  , _functionMap      :: HashMap Id EncodedFunction
  }
  deriving Eq

emptySearchState :: SearchState
emptySearchState = SearchState { _currentSolutions = []
                               , _currentLoc       = 0
                               , _currentSigs      = Map.empty
                               , _functionMap      = HashMap.empty
                               }

makeLenses ''SearchState

data CheckError = CheckError
  { errorProgram :: TProgram
  , desiredType  :: TypeSkeleton
  }
  deriving (Eq, Show)

type InstanceMapping = HashMap (Id, [TypeSkeleton]) (Id, TypeSkeleton)

data RefineState = RefineState
  { _abstractionCover :: AbstractCover
  , _instanceMapping  :: InstanceMapping
  , _instanceName     :: NameMapping
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
                               , _instanceName     = Map.empty
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
                               , _searchState  = emptySearchState
                               , _groupState   = emptyGroup
                               , _encoder      = emptyEncodeState
                               , _typeChecker  = emptyChecker
                               , _filterState  = emptyFilterState
                               }

makeLenses ''SolverState

instance Monad m => Fresh SolverState m where
  nextCounter prefix = do
    checker <- gets $ view typeChecker
    let counterMap = getCounter checker
    let i          = Map.findWithDefault 0 prefix counterMap
    modify $ set
      typeChecker
      (checker { getCounter = Map.insert prefix (i + 1) counterMap })
    return i

type PNSolver m = StateT SolverState m
type BackTrack m = LogicT (PNSolver m)
type SolverMonad m = (MonadIO m, MonadFail m)

instance Monad m => Loggable (PNSolver m) where
  getLogLevel = getExperiment logLevel

instance Monad m => Loggable (BackTrack m) where
  getLogLevel = lift getLogLevel

getExperiment exp = gets $ view (searchParams . exp)
