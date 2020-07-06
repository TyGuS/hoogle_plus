{-# LANGUAGE FlexibleInstances #-}
module Datalog.SouffleType where

import Database.Utils
import Datalog.DatalogType
import Datalog.Utils
import Types.Common
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

getArgs :: Int -> String -> String -> [String] -> [String]
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
    writeType (SoufflePack (TypeVarT id)) = id
    writeType (SoufflePack (DatatypeT dt)) =
        let name = replaceId tyclassPrefix "" dt
         in printf "TyAppT(\"%s\", \"\", \"%s\", 0, _)" name name
    writeType (SoufflePack t@(TyAppT tFun tArg)) =
        let tFun' = printf (writeType (SoufflePack tFun))
            tArg' = writeType (SoufflePack tArg)
        let (dt, args) = collectArgs t
            argStrs = foldr (\a acc -> printf "[%s, %s]" (writeType $ SoufflePack a) acc) "nil" args
         in printf "[\"%s\", %s]" (replaceId tyclassPrefix "" dt) argStrs
    writeType (SoufflePack (TyFunT tArg tRes)) = writeType (SoufflePack (TyAppT (TyAppT (DatatypeT "Fun") tArg) tRes))
    writeType (SoufflePack (FunctionT _ tArg tRes)) = writeType (SoufflePack (TyFunT tArg tRes))

    writeArg name (SoufflePack tArg) = printf "%s\t%s" (writeArg' tArg) name
        where
            writeArg' (TypeVarT id) = printf "[%s, nil]" id :: String
            writeArg' (DatatypeT dt) = replaceId tyclassPrefix "" dt
            writeArg' t@(TyAppT tFun tArg) = let (dt, args) = collectArgs t
                                                 argStrs = foldr (\a acc -> printf "[%s, %s]" (writeArg' a) acc) "nil" args
                                              in printf "[%s, %s]" (replaceId tyclassPrefix "" dt) argStrs
            writeArg' (TyFunT tArg tRes) = writeArg' (TyAppT (TyAppT (DatatypeT "Fun") tArg) tRes)
            writeArg' (FunctionT _ tArg tRes) = writeArg' (TyFunT tArg tRes)
