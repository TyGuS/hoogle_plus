{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Monad.Logic

import Types.Program
import Types.Abstract
import Types.Experiments hiding (PetriNet)
import Types.Type
import Types.Common
import Types.Encoder
import Types.HyperGraph

rootNode = AScalar (ATypeVarT varName)
pairProj = "pair_match"

type AbstractCover = HashMap AbstractSkeleton (Set AbstractSkeleton)

data ExplorerState = ExplorerState {
    _forwardSet :: Set RType,
    _backwardSet :: Set RType,
    _selectedTypes :: Set RType,
    _selectedNames :: Set Id,
    _selectedSymbols :: Map Id RType,
    _nullaries :: Map Id RSchema,
    _currentDepth :: Int
} deriving(Eq)

makeLenses ''ExplorerState

emptyExplorer = ExplorerState {
    _forwardSet = Set.empty,
    _backwardSet = Set.empty,
    _selectedTypes = Set.empty,
    _selectedNames = Set.empty,
    _selectedSymbols = Map.empty,
    _nullaries = Map.empty,
    _currentDepth = 0
}

data SolverState = SolverState {
    _searchParams :: SearchParams,
    _nameCounter :: Map Id Int,  -- name map for generating fresh names (type variables, parameters)
    _abstractionCover :: AbstractCover,
    _currentSolutions :: [RProgram], -- type checked solutions
    _currentLoc :: Int, -- current solution depth
    _currentSigs :: Map Id AbstractSkeleton, -- current type signature groups
    _functionMap :: HashMap Id FunctionCode,
    _targetType :: AbstractSkeleton,
    _sourceTypes :: [AbstractSkeleton],
    _mustFirers :: HashMap Id [Id],
    _groupMap :: Map GroupId (Set Id), -- mapping from group id to Skel and list of function names with the same skel
    _nameToGroup :: Map Id GroupId,
    _type2transition :: HashMap Int (Set Id), -- mapping from abstract type to group ids
    _solverStats :: TimeStatistics,
    _splitTypes :: Set AbstractSkeleton,
    _nameMapping :: Map Id Id, -- mapping from fake names to real names
    _instanceMapping :: HashMap (Id, [AbstractSkeleton]) (Id, AbstractSkeleton),
    _instanceCounts :: HashMap Id Int, -- Number of instantiations for a real-name, used in selecting representative
    _toRemove :: [Id],
    _useCount :: Map Id Int,
    _explorer :: ExplorerState,
    _typeToIndex :: Map AbstractSkeleton Int,
    _triedPaths :: Set [Id],
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
    _functionMap = HashMap.empty,
    _targetType = AScalar (ATypeVarT varName),
    _sourceTypes = [],
    _mustFirers = HashMap.empty,
    _groupMap = Map.empty,
    _nameToGroup = Map.empty,
    _type2transition = HashMap.empty,
    _solverStats = emptyTimeStats,
    _splitTypes = Set.empty,
    _nameMapping = Map.empty,
    _instanceMapping = HashMap.empty,
    _instanceCounts = HashMap.empty,
    _toRemove = [],
    _useCount = Map.empty,
    _explorer = emptyExplorer,
    _typeToIndex = Map.empty,
    _triedPaths = Set.empty,
    _messageChan = undefined
}

makeLenses ''SolverState

type PNSolver m = LogicT (StateT (SolverState, SearchState) m)

instance Freshable (SolverState, SearchState) where
    getNameIndices = _nameCounter . fst
    setNameIndices m (s, st) = (s { _nameCounter = m }, st)

instance Timable (SolverState, SearchState) where
    getTimeStats = _solverStats . fst
    setTimeStats m (s, st) = (s { _solverStats = m }, st)

type Constraints = [(RType, RType)]

data Option = NotFound | Found RProgram | MoreRefine (RProgram, AbstractSkeleton)