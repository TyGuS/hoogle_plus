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

import Types.Program
import Types.Abstract
import Types.Experiments hiding (PetriNet)
import Types.Type
import Types.Common

data SolverState = SolverState {
    _nameCounter :: Map Id Int,  -- name map for generating fresh names (type variables, parameters)
    _typeAssignment :: Map Id SType,  -- current type assignment for each type variable
    _abstractionTree :: Set AbstractSkeleton,
    _isChecked :: Bool, -- is the current state check passed
    _currentSolutions :: [RProgram], -- type checked solutions
    _currentLoc :: Int, -- current solution depth
    _currentSigs :: Map Id AbstractSkeleton, -- current type signature groups
    _detailedSigs :: Set Id,
    _functionMap :: HashMap Id FunctionCode,
    _targetType :: AbstractSkeleton,
    _sourceTypes :: [AbstractSkeleton],
    _mustFirers :: [Id],
    _paramNames :: [Id],
    _refineStrategy :: RefineStrategy,
    _groupMap :: Map Id [Id], -- mapping from group id to list of function names
    _type2transition :: Map AbstractSkeleton [Id], -- mapping from abstract type to group ids
    _solverStats :: TimeStatistics,
    _useGroup :: Bool,
    _splitTypes :: [AbstractSkeleton],
    _nameMapping :: Map Id Id, -- mapping from fake names to real names
    _logLevel :: Int, -- temporary for log level
    _instanceMapping :: HashMap (Id, [(Id, AbstractSkeleton)]) Id,
    _maxApplicationDepth :: Int
} deriving(Eq)


emptySolverState = SolverState {
    _nameCounter = Map.empty,
    _typeAssignment = Map.empty,
    _abstractionTree = Set.singleton (AScalar (ATypeVarT varName)),
    _isChecked = True,
    _currentSolutions = [],
    _currentLoc = 1,
    _currentSigs = Map.empty,
    _detailedSigs = Set.empty,
    _functionMap = HashMap.empty,
    _targetType = AScalar (ATypeVarT varName),
    _sourceTypes = [],
    _mustFirers = [],
    _paramNames = [],
    _refineStrategy = NoRefine,
    _groupMap = Map.empty,
    _type2transition = Map.empty,
    _solverStats = emptyTimeStats,
    _useGroup = False,
    _splitTypes = [],
    _nameMapping = Map.empty,
    _logLevel = 0,
    _instanceMapping = HashMap.empty,
    _maxApplicationDepth = 6
}

makeLenses ''SolverState

type PNSolver m = StateT SolverState m
