module Encoder.SouffleType where

import Types.Abstract
import Types.Type
import Datalog.SouffleType
import Datalog.DatalogType

import Control.Lens
import Text.Printf

{- turn types into souffle constraints -}
newtype SouffleAbs = SouffleAbs Int

instance PrintType SouffleAbs where
    writeType (SouffleAbs idx) = show idx
    writeArg name (SouffleAbs idx) = printf "%d\t%s" idx name

