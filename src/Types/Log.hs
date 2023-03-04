module Types.Log (
    Loggable(..)
  ) where

import Control.Monad.State
import Control.Monad.Except

class Monad m => Loggable m where
  getLogLevel :: m Int

instance Loggable m => Loggable (ExceptT e m) where
  getLogLevel = lift getLogLevel