module Types.Checker where

import Types.Common
import Types.Type

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

data CheckerState = CheckerState {
    typeAssignment :: Map Id RType,
    checkerNameMapping :: Map Id Id,
    isChecked :: Bool,
    checkerNameCounter :: Map Id Int
}

emptyCheckerState = CheckerState {
    typeAssignment = Map.empty,
    checkerNameMapping = Map.empty,
    isChecked = True,
    checkerNameCounter = Map.empty
}

type TypeChecker m = StateT CheckerState m

instance Freshable CheckerState where
    getNameIndices = checkerNameCounter
    setNameIndices m s = s { checkerNameCounter = m }