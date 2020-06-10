module Datalog.SouffleType where

import Text.Read
import Types.Type
import Types.Program
import Data.List
import Data.List.Extra (dropEnd)

instance Read UProgram where
    readsPrec _ ('[':input) = do 
        let inner = init input
        let (sym, remaining) = span ((/=) ',') inner
        let args = readList remaining :: [UProgram]
        if null args then return (Program (PSymbol sym) AnyT)
                     else return (Program (PApp sym args) AnyT)
