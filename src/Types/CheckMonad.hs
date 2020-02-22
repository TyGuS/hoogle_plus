module Types.CheckMonad where

import Types.Common
import Types.Type
import Types.Experiments

import Data.Map (Map)
import Control.Concurrent.Chan

class Monad m => CheckMonad m where
    getNameCounter :: m (Map Id Int)
    setNameCounter :: Map Id Int -> m ()
    getNameMapping :: m (Map Id Id)
    setNameMapping :: Map Id Id -> m ()
    getIsChecked :: m Bool
    setIsChecked :: Bool -> m ()
    getMessageChan :: m (Chan Message)
