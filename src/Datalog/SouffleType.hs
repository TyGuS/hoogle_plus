{-# LANGUAGE FlexibleInstances #-}
module Datalog.SouffleType where

import Database.Util
import Datalog.DatalogType
import Datalog.Utils
import Types.Type
import Types.Program
import HooglePlus.Utils
import Synquid.Type

import Text.Read
import Data.Char
import Data.List
import Data.List.Extra (dropEnd)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
import Text.Printf

getArgs 0 "" curr sofar = sofar
getArgs 0 (',':str) curr sofar = getArgs 0 (init $ drop 1 $ dropWhile isSpace str) "" (sofar ++ [curr])
getArgs i ('[':str) curr sofar = getArgs (i + 1) str (curr ++ "[") sofar
getArgs i (']':str) curr sofar = getArgs (i - 1) str (curr ++ "]") sofar
getArgs i (c:str) curr sofar = getArgs i str (curr ++ [c]) sofar
getArgs i str curr sofar = error $ show (i, str, curr, sofar)

instance Read UProgram where
    readsPrec _ ('[':input) = do
        let inner = init input
        let (sym, remaining) = span (',' /=) inner
        let next = init $ drop 1 $ dropWhile isSpace $ drop 1 remaining
        let nextArgs = getArgs 0 next "" []
        let args = map read nextArgs :: [UProgram]
        if null args
            then return (Program (PSymbol sym) AnyT, "")
            else return (Program (PApp sym args) AnyT, "")
    readsPrec _ _ = []

instance PrintType SoufflePack where
    writeType vars (SoufflePack (ScalarT (TypeVarT _ id) _)) = map toUpper id -- if id `Set.member` vars then map toUpper id else "_"
    writeType vars (SoufflePack (ScalarT (DatatypeT dt args _) _)) = printf "[\"%s\", %s]" (replaceId tyclassPrefix "" dt) argStrs
        where
            argStrs = foldr (\a acc -> printf "[%s, %s]" (writeType vars $ SoufflePack a) acc) "nil" args
    writeType vars (SoufflePack (FunctionT _ tArg tRes)) = writeType vars (SoufflePack $ ScalarT (DatatypeT "Fun" [tArg, tRes] []) ())

    writeArg name (SoufflePack tArg) = printf "%s\t%s" (writeArg' tArg) name
        where
            writeArg' (ScalarT (TypeVarT _ id) _) = printf "[%s, nil]" id :: String
            writeArg' (ScalarT (DatatypeT dt args _) _) = let argStrs = foldr (\a acc -> printf "[%s, %s]" (writeArg' a) acc) "nil" args
                                                           in printf "[\"%s\", %s]" (replaceId tyclassPrefix "" dt) argStrs
            writeArg' (FunctionT _ tArg tRes) = writeArg' (ScalarT (DatatypeT "Fun" [tArg, tRes] []) ())
