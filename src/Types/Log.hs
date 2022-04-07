module Types.Log (
    Loggable(..)
  ) where

class Monad m => Loggable m where
  getLogLevel :: m Int