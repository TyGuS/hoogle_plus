{-# LANGUAGE TemplateHaskell #-}
module Types.Solver where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Lens
import Control.Monad.State
import Control.Concurrent.Chan

import Types.Program
import Types.Abstract
import Types.Experiments hiding (PetriNet)
import Types.Type
import Types.Common
import Types.Encoder

rootNode = AScalar (ATypeVarT varName)
pairProj = "pair_match"

type AbstractCover = HashMap AbstractSkeleton (Set AbstractSkeleton)

data SolverState = SolverState {
    _searchParams :: SearchParams,
    _nameCounter :: Map Id Int,  -- name map for generating fresh names (type variables, parameters)
    _abstractionCover :: AbstractCover,
    _currentSolutions :: [RProgram], -- type checked solutions
    _currentLoc :: Int, -- current solution depth
    _currentSigs :: Map Id AbstractSkeleton, -- current type signature groups
    _activeSigs :: Set Id,
    _functionMap :: HashMap Id FunctionCode,
    _targetType :: AbstractSkeleton,
    _sourceTypes :: [AbstractSkeleton],
    _mustFirers :: HashMap Id [Id],
    _groupMap :: Map GroupId (Set Id), -- mapping from group id to Skel and list of function names with the same skel
    _groupRepresentative :: Map GroupId Id, -- mapping of current representative for group.
    _typeToGroup :: Map FunctionCode GroupId,
    _nameToGroup :: Map Id GroupId,
    _type2transition :: HashMap AbstractSkeleton (Set Id), -- mapping from abstract type to group ids
    _solverStats :: TimeStatistics,
    _splitTypes :: Set AbstractSkeleton,
    _nameMapping :: Map Id Id, -- mapping from fake names to real names
    _instanceMapping :: HashMap (Id, [AbstractSkeleton]) (Id, AbstractSkeleton),
    _instanceCounts :: HashMap Id Int, -- Number of instantiations for a real-name, used in selecting representative
    _toRemove :: [Id],
    _useCount :: Map Id Int,
    _messageChan :: Chan Message
} deriving(Eq)


emptySolverState :: SolverState
emptySolverState = SolverState {
    _searchParams = defaultSearchParams,
    _nameCounter = Map.empty,
    _abstractionCover = HashMap.singleton rootNode Set.empty,
    _currentSolutions = [],
    _currentLoc = 1,
    _currentSigs = Map.empty,
    _activeSigs = Set.empty,
    _functionMap = HashMap.empty,
    _targetType = AScalar (ATypeVarT varName),
    _sourceTypes = [],
    _mustFirers = HashMap.empty,
    _groupMap = Map.empty,
    _groupRepresentative = Map.empty,
    _typeToGroup = Map.empty,
    _nameToGroup = Map.empty,
    _type2transition = HashMap.empty,
    _solverStats = emptyTimeStats,
    _splitTypes = Set.empty,
    _nameMapping = Map.empty,
    _instanceMapping = HashMap.empty,
    _instanceCounts = HashMap.empty,
    _toRemove = [],
    _useCount = Map.empty,
    _messageChan = undefined
}

makeLenses ''SolverState

type PNSolver m = StateT SolverState m

instance Freshable SolverState where
    getNameIndices = _nameCounter
    setNameIndices m s = s { _nameCounter = m }

instance Timable SolverState where
    getTimeStats = _solverStats
    setTimeStats m s = s { _solverStats = m }

data BiSolverState = BiSolverState {
    _biSearchParams :: SearchParams,
    _allSymbols :: Map Id RSchema,
    _selectedTypes :: Set RType,
    _selectedSymbols :: Map Id RType,
    _petrinet :: HashMap AbstractSkeleton (Set Id),
    _nameMap :: Map Id Id,
    _forwardSet :: Set RType,
    _backwardSet :: Set RType,
    _srcTypes :: [RType],
    _dstTypes :: [RType],
    _maxLength :: Int,
    _biNameCounter :: Map Id Int,
    _biTimeStats :: TimeStatistics
}

emptyBiSolver :: BiSolverState
emptyBiSolver = BiSolverState {
    _biSearchParams = defaultSearchParams,
    _allSymbols = Map.empty,
    _selectedTypes = Set.empty,
    _selectedSymbols = Map.empty,
    _petrinet = HashMap.empty,
    _nameMap = Map.empty,
    _forwardSet = Set.empty,
    _backwardSet = Set.empty,
    _srcTypes = [],
    _dstTypes = [],
    _maxLength = 0,
    _biNameCounter = Map.empty,
    _biTimeStats = emptyTimeStats
}

makeLenses ''BiSolverState

instance Freshable BiSolverState where
    getNameIndices = _biNameCounter
    setNameIndices m s = s { _biNameCounter = m }

instance Timable BiSolverState where
    getTimeStats = _biTimeStats
    setTimeStats m s = s { _biTimeStats = m }

type BiSolver m = StateT BiSolverState m