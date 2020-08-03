module Datalog.DatalogType where

import Types.Common
import Types.Type
import Synquid.Pretty

import Data.Set (Set)
import Text.Printf
import Control.Monad.State

newtype SoufflePack = SoufflePack TypeSkeleton
newtype FormulogPack = FormulogPack TypeSkeleton

class PrintType a where
    writeType :: String -> a -> State Int (String, String)