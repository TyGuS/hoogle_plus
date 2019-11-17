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
    _typeAssignment :: Map Id SType,  -- current type assignment for each type variable
    _abstractionCover :: AbstractCover,
    _isChecked :: Bool, -- is the current state check passed
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
    _encoder :: EncodeState,
    _messageChan :: Chan Message
}


emptySolverState :: SolverState
emptySolverState = SolverState {
    _searchParams = defaultSearchParams,
    _nameCounter = Map.empty,
    _typeAssignment = Map.empty,
    _abstractionCover = HashMap.singleton rootNode Set.empty,
    _isChecked = True,
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
    _encoder = undefined,
    _messageChan = undefined
}

makeLenses ''SolverState

type PNSolver m = StateT SolverState m
