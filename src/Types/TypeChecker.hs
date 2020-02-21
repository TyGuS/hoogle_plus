module Types.TypeChecker where

import Types.Type

import Data.Map (Map)
import Control.Monad.State

data CheckerState = CheckerState {
    _nameCounter :: Map Id Int,
    _isChecked :: Bool,
    _typeAssignment :: Map Id RType
} deriving(Eq, Ord, Show)

emptyChecker = CheckerState {
    _nameCounter = Map.empty,
    _isChecked = True,
    _typeAssignment = Map.empty
}

type Checker = StateT CheckerState
