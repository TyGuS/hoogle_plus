{-# LANGUAGE FlexibleInstances #-}
module Datalog.FormulogType where

import Database.Utils
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
getArgs i ('(':str) curr sofar = getArgs (i + 1) str (curr ++ "(") sofar
-- getArgs i str curr sofar | ("exp_var(" `isPrefixOf` str) || ("exp_app(" `isPrefixOf` str) = 
--    getArgs (i + 1) (drop 8 str) (curr ++ take 8 str) sofar
getArgs i (')':str) curr sofar = getArgs (i - 1) str (curr ++ ")") sofar
getArgs i (c:str) curr sofar = getArgs i str (curr ++ [c]) sofar
getArgs i str curr sofar = error $ show (i, str, curr, sofar)

instance Read UProgram where
    readsPrec p input | "query(" `isPrefixOf` input =
        let inner = init (drop 6 input)
         in return (read inner :: UProgram, "")
    readsPrec _ input | "exp_var(" `isPrefixOf` input =
        let sym = read (init (drop 8 input)) :: String
         in return (Program (PSymbol sym) AnyT, "")
    readsPrec _ input | "exp_app(" `isPrefixOf` input = do
        let inner = init (drop 8 input)
        let (sym, remaining) = span (',' /=) inner
        let argsStr = init $ drop 1 $ dropWhile isSpace $ drop 1 remaining
        let argsInner = getArgs 0 argsStr "" []
        let args = map read argsInner :: [UProgram]
        if null args
            then return (Program (PSymbol sym) AnyT, "")
            else return (Program (PApp sym args) AnyT, "")
    readsPrec _ _ = []

instance PrintType FormulogPack where
    writeType (FormulogPack (TypeVarT id)) = map toUpper id
    writeType (FormulogPack (DatatypeT dt)) = replaceId tyclassPrefix "" dt
    writeType (FormulogPack t@TyAppT {}) = let (dt, args) = collectArgs t
                                               argStrs = foldr (\a acc -> printf "%s :: %s" (writeType $ FormulogPack a) acc) "[]" args
                                            in printf "typ_app(\"%s\", %s)" (replaceId tyclassPrefix "" dt) argStrs
    writeType (FormulogPack (TyFunT tArg tRes)) = writeType (FormulogPack (TyAppT (TyAppT (DatatypeT "Fun") tArg) tRes))
    writeType (FormulogPack (FunctionT _ tArg tRes)) = writeType (FormulogPack (TyFunT tArg tRes))
