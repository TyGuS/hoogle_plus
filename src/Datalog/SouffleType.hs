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

import Text.Read hiding (get)
import Data.Char
import Data.List
import Data.List.Extra (dropEnd)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
import Text.Printf
import Control.Monad.State

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
    writeType _ (SoufflePack (TypeVarT id)) = return (id, "")
    writeType _ (SoufflePack (DatatypeT dt)) = do
        let name = replaceId tyclassPrefix "" dt
        return (name, "")
    writeType pre (SoufflePack t@TyAppT {}) = do
        let (dt, args) = collectArgs t
        idx <- get
        put (idx + 1)
        let myPrefix = pre ++ show idx
        let writeArg pre = writeType pre . SoufflePack
        -- start a new naming index in the inner level
        let (argPairs, innerIdx) = runState (mapM (writeArg myPrefix) args) 0
        let (argTyps, argStrs) = unzip argPairs
        let name = replaceId tyclassPrefix "" dt
        let argNum = length args
        let intermediateType i = pre ++ show (i + innerIdx) 
        let argApps = zipWith (\arg i -> printf "TyApp(%s, %s, %s, %d, _)"
                                    -- assgin new indices to intermediate types
                                    -- for example, Pair T1 T2 is writter as
                                    -- TyApp("Pair", T1, T3, 1, _)
                                    -- TyApp(T3, T2, T0, 0, _)
                                    (if i == 0 then show name else intermediateType (i - 1)) 
                                    arg
                                    (if i == argNum - 1 then pre else intermediateType i) 
                                    (argNum - i - 1) -- kind of the type
                                    ) argTyps [0 .. (argNum - 1)]
        return (pre, intercalate ", " (if innerIdx /= 0 then argStrs else [] ++ argApps))
    writeType pre (SoufflePack (TyFunT tArg tRes)) = writeType pre (SoufflePack (TyAppT (TyAppT (DatatypeT "Fun") tArg) tRes))
    writeType pre (SoufflePack (FunctionT _ tArg tRes)) = writeType pre (SoufflePack (TyFunT tArg tRes))
    writeType _ _ = error "Should not see BottomT or AnyT during environment generation"
