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
    writeType _ (SoufflePack (TypeVarT id)) = return id
    writeType _ (SoufflePack (DatatypeT dt)) = do
        let name = replaceId tyclassPrefix "" dt
        return $ printf "TyApp(\"%s\", \"\", \"%s\", 0, _)" name name
    writeType pre (SoufflePack t@(TyAppT tFun tArg)) = do
        let (dt, args) = collectArgs t
        idx <- get
        put (idx + 1)
        let myPrefix = pre ++ show idx
        let writeArg pre = writeType pre . SoufflePack
        -- start a new naming index in the inner level
        let (argStrs, innerIdx) = runState (mapM (writeArg myPrefix) args) 0
        let name = replaceId tyclassPrefix "" dt
        let argNum = length args
        let argApps = map (\i -> printf "TyApp(%s, %s%d, %s%d, %d, _)" (if i == 0 then show name else (myPrefix ++ show (i - 1))) myPrefix (i + innerIdx) myPrefix i (argNum - i - 1)) [0 .. argNum]
        return $ intercalate ", " (argStrs ++ argApps)
    writeType pre (SoufflePack (TyFunT tArg tRes)) = writeType pre (SoufflePack (TyAppT (TyAppT (DatatypeT "Fun") tArg) tRes))
    writeType pre (SoufflePack (FunctionT _ tArg tRes)) = writeType pre (SoufflePack (TyFunT tArg tRes))
