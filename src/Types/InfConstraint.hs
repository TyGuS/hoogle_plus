{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.InfConstraint where

import Types.Type
import Types.CheckMonad
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
    _tyclassCache :: Map SType [Id],
    _supportModules :: [String],
    -- stats
    _infStats :: InfStats
} deriving(Eq)

makeLenses ''TypeClassState

emptyTyclassState = TypeClassState {
    _tyclassCache = Map.empty,
    _supportModules = [],
    _infStats = InfStats 0 0
}

data AntiUnifState = AntiUnifState {
    _generalNames :: Map Id Int,
    _typeAssignment1 :: Map SType [Id],
    _typeAssignment2 :: Map SType [Id],
    _tyclassAssignment :: TyclassAssignment
} deriving(Eq)

makeLenses ''AntiUnifState

emptyAntiUnifState = AntiUnifState {
    _generalNames = Map.empty,
    _typeAssignment1 = Map.empty,
    _typeAssignment2 = Map.empty,
    _tyclassAssignment = Map.empty
}

data TypeNaming = TypeNaming {
    _substCounter :: Map SType (Id, Int),
    _nameCounter :: Map Id Int,
    _prevTypeVars :: Set Id,
    _beginTypeVars :: Set Id
} deriving(Eq)

makeLenses ''TypeNaming

type AntiUnifier m = StateT AntiUnifState (StateT TypeClassState m)
type TypeGeneralizer m = StateT TypeNaming (LogicT (StateT TypeClassState m))

instance Monad m => CheckMonad (AntiUnifier m) where
    getNameCounter = gets (view generalNames)
    setNameCounter nc = modify (set generalNames nc)
    getNameMapping = getNameMapping
    setNameMapping = setNameMapping
    getIsChecked = getIsChecked
    setIsChecked = setIsChecked
    getMessageChan = getMessageChan
    overStats = overStats

instance Monad m => CheckMonad (TypeGeneralizer m) where
    getNameCounter = gets (view nameCounter)
    setNameCounter nc = modify (set nameCounter nc)
    getNameMapping = getNameMapping
    setNameMapping = setNameMapping
    getIsChecked = getIsChecked
    setIsChecked = setIsChecked
    getMessageChan = getMessageChan
    overStats = overStats

univTypeVarPrefix = '?'
