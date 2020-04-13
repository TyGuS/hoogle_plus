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

data TypeClassState = TypeClassState {
    _tyclassCache :: Map SType [Id],
    _supportModules :: [String]
} deriving(Eq)

makeLenses ''TypeClassState

emptyTyclassState = TypeClassState {
    _tyclassCache = Map.empty,
    _supportModules = []
}

data AntiUnifState = AntiUnifState {
    _generalNames :: Map Id Int,
    _typeAssignment1 :: Map SType [Id],
    _typeAssignment2 :: Map SType [Id],
    _tyclassAssignment :: TyclassAssignment
} deriving(Eq)

makeLenses ''AntiUnifState

instance Monad m => CheckMonad (AntiUnifier m) where
    getNameCounter = gets (view generalNames)
    setNameCounter nc = modify (set generalNames nc)
    getNameMapping = getNameMapping
    setNameMapping = setNameMapping
    getIsChecked = getIsChecked
    setIsChecked = setIsChecked
    getMessageChan = getMessageChan
    overStats = overStats

emptyAntiUnifState = AntiUnifState {
    _generalNames = Map.empty,
    _typeAssignment1 = Map.empty,
    _typeAssignment2 = Map.empty,
    _tyclassAssignment = Map.empty
}

type TypeNaming = Map SType (Id, Int)
type AntiUnifier m = StateT AntiUnifState (StateT TypeClassState m)
type TypeGeneralizer m = StateT TypeNaming (LogicT (StateT TypeClassState m))

univTypeVarPrefix = '?'
