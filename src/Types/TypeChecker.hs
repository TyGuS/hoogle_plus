{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
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
    _typeAssignment :: Map Id TypeSkeleton,
    _nameMapping :: Map Id Id,
    _checkerLogLevel :: Int
} deriving(Eq)

emptyChecker :: CheckerState
emptyChecker = CheckerState {
    _nameCounter = Map.empty,
    _isChecked = True,
    _typeAssignment = Map.empty,
    _nameMapping = Map.empty,
    _checkerLogLevel = 0
}

makeLenses ''CheckerState

type CheckerIO m = StateT CheckerState m
type Checker = State CheckerState

instance Monad m => CheckMonad (CheckerIO m) where
    getNameCounter = gets (view nameCounter)
    setNameCounter nc = modify (set nameCounter nc)
    getNameMapping = gets (view nameMapping)
    setNameMapping nm = modify (set nameMapping nm)
    getIsChecked = gets (view isChecked)
    setIsChecked c = modify (set isChecked c)
    getLogLevel = gets (view checkerLogLevel)
    overStats = overStats
