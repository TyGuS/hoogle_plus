{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Types.Solver where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Lens
import Control.Monad.State
import Control.Concurrent.Chan
import Control.Monad.Logic

import Database.Utils
import Encoder.ConstraintEncoder
import Types.CheckMonad
import Types.Common
import Types.Experiments hiding (PetriNet)
import Types.Filtering
import Types.IOFormat
import Types.Program
import Types.Type
import Types.TypeChecker

rootNode = TypeVarT varName
pairProj = "pair_match"

type AbstractCover = HashMap AbstractSkeleton (Set AbstractSkeleton)

data GroupState = GroupState {
    _groupMap :: Map GroupId (Set Id), -- mapping from group id to Skel and list of function names with the same skel
    _groupRepresentative :: Map GroupId Id, -- mapping of current representative for group.
    _typeToGroup :: Map FunctionCode GroupId,
    _nameToGroup :: Map Id GroupId
} deriving(Eq)

emptyGroup = GroupState {
    _groupMap = Map.empty,
    _groupRepresentative = Map.empty,
    _typeToGroup = Map.empty,
    _nameToGroup = Map.empty
}

makeLenses ''GroupState

data SearchState = SearchState {
    _currentSolutions :: [TProgram], -- type checked solutions
    _currentLoc :: Int, -- current solution depth
    _currentSigs :: Map Id AbstractSkeleton, -- current type signature groups
    _activeSigs :: Set Id,
    _functionMap :: HashMap Id FunctionCode
} deriving(Eq)

emptySearchState = SearchState {
    _currentSolutions = [],
    _currentLoc = 0,
    _currentSigs = Map.empty,
    _activeSigs = Set.empty,
    _functionMap = HashMap.empty
}

makeLenses ''SearchState

data StatisticState = StatisticState {
    _instanceCounts :: HashMap Id Int, -- Number of instantiations for a real-name, used in selecting representative
    _useCount :: Map Id Int,
    _solverStats :: TimeStatistics
} deriving(Eq)

emptyStatistic = StatisticState {
    _instanceCounts = HashMap.empty,
    _useCount = Map.empty,
    _solverStats = emptyTimeStats
}

makeLenses ''StatisticState

type CheckError = (TProgram, AbstractSkeleton)
type InstanceMap = HashMap (Id, AbsArguments) (Id, AbsReturn)

data RefineState = RefineState {
    _abstractionCover :: AbstractCover,
    _instanceMapping :: InstanceMap,
    _targetType :: AbstractSkeleton,
    _sourceTypes :: [AbstractSkeleton],
    _splitTypes :: Set AbstractSkeleton,
    _toRemove :: [Id],
    _passOneOrMore :: Bool, -- ^ whether all the possible filling of a sketch all type checks
    _lastError :: CheckError -- ^ the last type checking error, used for refinement
} deriving(Eq)

emptyRefineState = RefineState {
    _abstractionCover = HashMap.empty,
    _instanceMapping = HashMap.empty,
    _targetType = TypeVarT varName,
    _sourceTypes = [],
    _splitTypes = Set.empty,
    _toRemove = [],
    _passOneOrMore = False,
    _lastError = undefined
}

makeLenses ''RefineState

data SolverState enc = SolverState {
    _searchParams :: SearchParams,
    _refineState :: RefineState,
    _statistics :: StatisticState,
    _searchState :: SearchState,
    _groupState :: GroupState,
    _encoder :: ConstraintEncoder enc => enc,
    _typeChecker :: CheckerState,
    _filterState :: FilterState
}

emptySolverState :: ConstraintEncoder enc => SolverState enc
emptySolverState = SolverState {
    _searchParams = defaultSearchParams,
    _refineState = emptyRefineState,
    _statistics = emptyStatistic,
    _searchState = emptySearchState,
    _groupState = emptyGroup,
    _encoder = emptyEncoder,
    _typeChecker = emptyChecker,
    _filterState = emptyFilterState
}

makeLenses ''SolverState

type PNSolver enc m = StateT (SolverState enc) m
type BackTrack enc m = LogicT (PNSolver enc m)

instance (ConstraintEncoder enc, Monad m) => CheckMonad (PNSolver enc m) where
    getNameCounter = gets (view (typeChecker . nameCounter))
    setNameCounter nc = modify (set (typeChecker . nameCounter) nc)
    getNameMapping = gets (view (typeChecker . nameMapping))
    setNameMapping nm = modify (set (typeChecker . nameMapping) nm)
    getIsChecked = gets (view (typeChecker . isChecked))
    setIsChecked c = modify (set (typeChecker . isChecked) c)
    getLogLevel = gets (view (searchParams . explorerLogLevel))
    overStats f = modify (over (statistics . solverStats) f)

instance (ConstraintEncoder enc, Monad m) => CheckMonad (BackTrack enc m) where
    getNameCounter = gets (view (typeChecker . nameCounter))
    setNameCounter nc = modify (set (typeChecker . nameCounter) nc)
    getNameMapping = gets (view (typeChecker . nameMapping))
    setNameMapping nm = modify (set (typeChecker . nameMapping) nm)
    getIsChecked = gets (view (typeChecker . isChecked))
    setIsChecked c = modify (set (typeChecker . isChecked) c)
    getLogLevel = gets (view (searchParams . explorerLogLevel))
    overStats f = modify (over (statistics . solverStats) f)

data SearchResult = NotFound 
                  | Found (TProgram, AssociativeExamples)
                  | MoreRefine (TProgram, AbstractSkeleton)
                  deriving(Eq)
