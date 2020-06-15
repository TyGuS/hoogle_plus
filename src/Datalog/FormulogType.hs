{-# LANGUAGE FlexibleInstances #-}
module Datalog.FormulogType where

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
    writeType vars (FormulogPack (ScalarT (TypeVarT _ id) _)) = if id `Set.member` vars then map toUpper id else "_"
    writeType vars (FormulogPack (ScalarT (DatatypeT dt args _) _)) = printf "typ_app(\"%s\", %s)" (replaceId tyclassPrefix "" dt) argStrs
        where
            argStrs = foldr (\a acc -> printf "%s :: %s" (writeType vars $ FormulogPack a) acc) "[]" args
    writeType vars (FormulogPack (FunctionT _ tArg tRes)) = writeType vars (FormulogPack $ ScalarT (DatatypeT "Fun" [tArg, tRes] []) ())

    writeArg name t@(FormulogPack tArg) =
        let vars = typeVarsOf tArg
            substedType = varToDatatype tArg
         in printf "%s\t%s" (writeType vars (FormulogPack substedType)) name
