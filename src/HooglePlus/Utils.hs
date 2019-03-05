{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.Utils where

import HooglePlus.Types

import Control.Monad.State
import Control.Lens
import Control.Monad
import Debug.Trace
import Text.PrettyPrint.ANSI.Leijen (plain)

writeLog level msg = do
    st <- get
    when (level <= st ^. logLevel) (traceShow (plain msg) $ return ())