{-# LANGUAGE TemplateHaskell #-}

module Types.InfConstraint where

import Types.Type
import Types.CheckMonad
import Types.Common
import Types.Experiments

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Control.Concurrent.Chan

data AntiUnifState = AntiUnifState {
    _generalNames :: Map Id Int,
    _typeAssignment1 :: Map SType [Id],
    _typeAssignment2 :: Map SType [Id],
    _unifChan :: Chan Message
} deriving(Eq)

makeLenses ''AntiUnifState

type AntiUnifier m = StateT AntiUnifState m

instance Monad m => CheckMonad (AntiUnifier m) where
    getNameCounter = gets (view generalNames)
    setNameCounter nc = modify (set generalNames nc)
    getNameMapping = getNameMapping
    setNameMapping = setNameMapping
    getIsChecked = getIsChecked
    setIsChecked = setIsChecked
    getMessageChan = gets (view unifChan)
    overStats = overStats

emptyAntiUnifState = AntiUnifState {
    _generalNames = Map.empty,
    _typeAssignment1 = Map.empty,
    _typeAssignment2 = Map.empty,
    _unifChan = undefined
}
