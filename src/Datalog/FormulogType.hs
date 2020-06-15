{-# LANGUAGE FlexibleInstances #-}
module Datalog.FormulogType where

import Datalog.DatalogType
import Types.Type
import Types.Program

import Text.Read
import Data.Char
import Data.List
import Data.List.Extra (dropEnd)
import Debug.Trace

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
        let argsInner = getArgs 0 next "" []
        let args = map read argsInner :: [UProgram]
        if null args
            then return (Program (PSymbol sym) AnyT, "")
            else return (Program (PApp sym args) AnyT, "")
    readsPrec _ _ = []

instance PrintType FormulogType where
    writeType vars (FormulogType (ScalarT (TypeVarT _ id) _)) = if id `Set.member` vars then map toUpper id else "_"
    writeType vars (FormulogType (ScalarT (DatatypeT dt args _) _)) = printf "[\"%s\", %s]" (replaceId tyclassPrefix "" dt) argStrs
        where
            argStrs = foldr (\a acc -> printf "[%s, %s]" (writeType vars a) acc) "nil" args
    writeType vars (FormulogType (FunctionT _ tArg tRes)) = writeType vars (FormulogType $ ScalarT (DatatypeT "Fun" [tArg, tRes] []) ())

    writeArg name t@(FormulogType tArg) = printf "inh(%s, \"%s\")" (writeType (typeVarsOf tArg) t) name
