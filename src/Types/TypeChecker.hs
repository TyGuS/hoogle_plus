{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Types.TypeChecker where

import Types.Common
import Types.Type
import Types.Experiments
import Types.CheckMonad

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Lens
import Control.Concurrent.Chan

data CheckerState = CheckerState {
    _nameCounter :: Map Id Int,
    _isChecked :: Bool,
    _typeAssignment :: Map Id SType,
    _nameMapping :: Map Id Id,
    _checkerChan :: Chan Message
} deriving(Eq)

emptyChecker :: CheckerState
emptyChecker = CheckerState {
    _nameCounter = Map.empty,
    _isChecked = True,
    _typeAssignment = Map.empty,
    _nameMapping = Map.empty,
    _checkerChan = undefined
}

makeLenses ''CheckerState

type Checker m = StateT CheckerState m

instance Monad m => CheckMonad (Checker m) where
    getNameCounter = gets (view nameCounter)
    setNameCounter nc = modify (set nameCounter nc)
    getNameMapping = gets (view nameMapping)
    setNameMapping nm = modify (set nameMapping nm)
    getIsChecked = gets (view isChecked)
    setIsChecked c = modify (set isChecked c)
    getMessageChan = gets (view checkerChan)
    overStats = overStats

data AntiUnifState = AntiUnifState {
    _generalNames :: Map Id Int,
    _typeAssignment1 :: Map SType Id,
    _typeAssignment2 :: Map SType Id,
    _unifChan :: Chan Message
} deriving(Eq)

makeLenses ''AntiUnifState

type AntiUnifier m = StateT AntiUnifState m

instance Monad m => CheckMonad (AntiUnifier m) where
    getNameCounter = gets (view generalNames)
    setNameCounter nc = modify (set generalName nc)
    getNameMapping = getNameMapping
    setNameMapping = setNameMapping
    getIsChecked = getIsChecked
    setIsChecked c = setIsChecked
    getMessageChan = gets (view unifChan)
    overStats = overStats

emptyAntiUnifState = AntiUnifState {
    _generalNames = Map.empty,
    _typeAssignment1 = Map.empty,
    _typeAssignment2 = Map.empty,
    _unifChan = undefined
}
