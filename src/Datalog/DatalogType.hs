module Datalog.DatalogType where

import Types.Common
import Types.Type
import Synquid.Pretty

import Data.Set (Set)
import Text.Printf

newtype SoufflePack = SoufflePack SType
newtype FormulogPack = FormulogPack SType

class PrintType a where
    writeType :: a -> String
    writeArg :: Id -> a -> String
