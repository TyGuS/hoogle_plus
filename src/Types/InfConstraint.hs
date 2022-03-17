{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.InfConstraint where

import Types.Type
import Types.Common
import Types.Experiments

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Control.Lens
import Control.Concurrent.Chan
import Control.Monad.State
import Control.Monad.Logic

type TyclassConstraints = Set Id
type TyclassAssignment = Map Id TyclassConstraints

data InfStats = InfStats {
    _prefilterCounts :: Int,
    _postfilterCounts :: Int
} deriving(Eq, Show)

makeLenses ''InfStats

data TypeClassState = TypeClassState {
    _tyclassCache :: Map TypeSkeleton [Id],
    _supportModules :: [String],
    _generalNames :: Map Id Int,
    -- stats
    _infStats :: InfStats
} deriving(Eq)

makeLenses ''TypeClassState

emptyTyclassState = TypeClassState {
    _tyclassCache = Map.empty,
    _supportModules = [],
    _generalNames = Map.empty,
    _infStats = InfStats 0 0
}

type AntiPair = (TypeSkeleton, TypeSkeleton)
data AntiUnifConstraint = 
    UnifConstraint TypeSkeleton TypeSkeleton
  | DisunifConstraint TypeSkeleton TypeSkeleton
  deriving(Eq)

data AntiUnifState = AntiUnifState {
    _typeAssignment :: Map AntiPair Id,
    _tyclassAssignment :: TyclassAssignment,
    -- temporary constraints to be satisfied during anti-unification
    _unifConstraints :: [AntiUnifConstraint],
    _disunifConstraints :: [AntiUnifConstraint],
    _tmpAssignment :: Map Id TypeSkeleton
} deriving(Eq)

makeLenses ''AntiUnifState

emptyAntiUnifState = AntiUnifState {
    _typeAssignment = Map.empty,
    _tyclassAssignment = Map.empty,
    _unifConstraints = [],
    _disunifConstraints = [],
    _tmpAssignment = Map.empty
}

data TypeNaming = TypeNaming {
    _substCounter :: Map TypeSkeleton (Id, Int),
    _nameCounter :: Map Id Int,
    _prevTypeVars :: Set Id,
    _beginTypeVars :: Set Id
} deriving(Eq)

makeLenses ''TypeNaming

type AntiUnifier m = StateT AntiUnifState (LogicT (StateT TypeClassState m))
type TypeGeneralizer m = StateT TypeNaming (LogicT (StateT TypeClassState m))
