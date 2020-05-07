{-# LANGUAGE FlexibleContexts #-}

module HooglePlus.Utils where

import Types.Common
import Types.Environment
import Types.Program
import Types.Type
import Types.Experiments
import Types.Solver
import qualified Types.TypeChecker as Checker
import Types.Filtering
import Types.IOFormat (Example(Example))
import qualified Types.IOFormat as IOFormat
import Synquid.Type
import Synquid.Util hiding (fromRight)
import Synquid.Pretty as Pretty
import Synquid.Program
import Database.Util

import Control.Exception
import Control.Monad.Trans
import Control.Monad.State
import CorePrep
import CoreSyn
import Data.Data
import Data.Ord
import Data.Either
import Data.List (sortOn, groupBy, isInfixOf, isPrefixOf, isSuffixOf, intercalate)
import Data.List.Extra (nubOrdOn, dropEnd)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Typeable
import Data.Function (on)
import qualified Data.Text as Text
import Demand
import DmdAnal
import DynFlags
import FamInstEnv
import GHC hiding (Id)
import GHC.Paths ( libdir )
import HscTypes
import IdInfo
import Outputable hiding (text, (<+>))
import qualified CoreSyn as Syn
import qualified Data.Map as Map hiding (map, foldr)
import qualified Data.Set as Set hiding (map)
import qualified Data.Text as Text
import SimplCore (core2core)
import System.Directory (removeFile)
import Text.Printf
import Text.Regex
import Var hiding (Id)
import Data.UUID.V4
import Debug.Trace
import qualified Language.Haskell.Interpreter as LHI

-- Converts the list of param types into a haskell function signature.
-- Moves typeclass-looking things to the front in a context.
mkFunctionSigStr :: Show (TypeSkeleton r) => [TypeSkeleton r] -> String
mkFunctionSigStr args = addConstraints $ Prelude.foldr accumConstraints ([],[]) args
    where
        showSigs = intercalate " -> "
        wrapParen x = "(" ++ x ++ ")"
        addConstraints ([], baseSigs) = showSigs baseSigs
        addConstraints (constraints, baseSigs) = "(" ++ (intercalate ", " constraints) ++ ") => " ++ showSigs baseSigs

        accumConstraints :: Show (TypeSkeleton r) => TypeSkeleton r -> ([String], [String]) -> ([String], [String])
        accumConstraints (ScalarT (DatatypeT id [ScalarT (TypeVarT _ tyvarName) _] _) _) (constraints, baseSigs)
            | tyclassPrefix `isPrefixOf` id = let
                classNameRegex = mkRegex $ tyclassPrefix ++ "([a-zA-Z]*)"
                className = subRegex classNameRegex id "\\1"
                constraint = className ++ " " ++ tyvarName
                -- \(@@hplusTC@@([a-zA-Z]*) \(([a-z]*)\)\)
                in
                    (constraint:constraints, baseSigs)
        accumConstraints otherTy (constraints, baseSigs) = let
            otherStr = if isFunctionType otherTy then wrapParen (show otherTy) else show otherTy
            in (constraints, otherStr:baseSigs)

-- mkLambdaStr produces a oneline lambda expr str:
-- (\x y -> body))
mkLambdaStr :: [String] -> UProgram -> String
mkLambdaStr args body =
    let nontcArgs = filter (not . (tyclassArgBase `isPrefixOf`)) args
        argStr = unwords nontcArgs
        unTypeclassed = toHaskellSolution (show body)
     in printf "\\%s -> %s" argStr unTypeclassed

toHaskellSolution :: String -> String
toHaskellSolution bodyStr = let
    oneLineBody = unwords $ lines bodyStr
    noTypeclasses = removeTypeclasses oneLineBody
    in
        noTypeclasses

removeAll :: Regex -> String -> String
removeAll a b = unwords $ words $ go a b
    where
        go regex input =
            if (isJust $ matchRegex regex input)
            then (go regex $ subRegex regex input "")
            else input

removeTypeclassArgs :: String -> String
removeTypeclassArgs = removeAll (mkRegex (tyclassArgBase++"[0-9]+"))

removeTypeclassInstances :: String -> String
removeTypeclassInstances = removeAll (mkRegex (tyclassInstancePrefix ++ "[0-9]*[a-zA-Z]*"))

removeTypeclasses = removeEmptyParens . removeTypeclassArgs . removeTypeclassInstances
    where
        removeEmptyParens = removeAll (mkRegex "\\(\\ +\\)")

printSolution solution = do
    putStrLn "*******************SOLUTION*********************"
    putStrLn $ "SOLUTION: " ++ toHaskellSolution (show solution)
    putStrLn "************************************************"

collectExamples :: String -> FilterState -> AssociativeExamples
collectExamples solution (FilterState _ sols samples examples) =
    map mkGroup $ groupBy (\x y -> fst x == fst y)
                $ sortOn fst
                $ examples ++ checkedExs
    where
        [(_, desc)] = filter ((== solution) . fst) samples
        checkedExs = zip (repeat solution) (descToExample desc)
        mkGroup xs = (fst (head xs), nubOrdOn IOFormat.inputs $ map snd xs)


descToExample :: FunctionCrashDesc -> [Example]
descToExample (AlwaysSucceed ex) = [ex]
descToExample (AlwaysFail ex) = [ex]
descToExample (PartialFunction exs) = exs
descToExample _ = []


-- printSolutionState solution fs = unlines ["****************", solution, show fs, "***********"]
printSolutionState solution (FilterState _ sols workingExamples diffExamples) = unlines [ios, diffs]
    where
        ios = let [(_, desc)] = filter ((== solution) . fst) workingExamples in show desc
        diffs = let examples = groupBy ((==) `on` fst) (sortOn fst diffExamples) in unlines (map showGroup examples)
        
        showGroup :: [(String, Example)] -> String
        showGroup xs = unlines ((fst $ head xs) : (map (show . snd) xs))

extractSolution :: Environment -> RType -> UProgram -> ([String], String, String, [(Id, RSchema)])
extractSolution env goalType prog = (modules, funcSig, body, argList)
    where
        args = _arguments env
        modules = "Prelude" : Set.toList (_included_modules env)
        argList = Map.toList args
        argNames = map fst argList
        argTypes = map snd argList
        monoGoals = map toMonotype argTypes
        funcSig = mkFunctionSigStr (monoGoals ++ [goalType])
        body = mkLambdaStr argNames prog

updateEnvWithBoundTyVars :: RSchema -> Environment -> (Environment, RType)
updateEnvWithBoundTyVars (Monotype ty) env = (env, ty)
updateEnvWithBoundTyVars (ForallT x ty) env = updateEnvWithBoundTyVars ty (addTypeVar x env)

updateEnvWithSpecArgs :: RType -> Environment -> (Environment, RType)
updateEnvWithSpecArgs ty@(ScalarT _ _) env = (env, ty)
updateEnvWithSpecArgs (FunctionT x tArg tRes) env = updateEnvWithSpecArgs tRes $ addVariable x tArg $ addArgument x tArg env

preprocessEnvFromGoal :: Goal -> (Environment, RType)
preprocessEnvFromGoal goal = updateEnvWithSpecArgs monospec env''
    where
        env''' = gEnvironment goal
        (env'', monospec) = updateEnvWithBoundTyVars (gSpec goal) env'''

replaceId a b = Text.unpack . Text.replace (Text.pack a) (Text.pack b) . Text.pack

matchNiceFunctions :: String -> StateT [(String, String)] IO String
matchNiceFunctions prog | null prog = return prog
matchNiceFunctions prog | head prog == '[' && last prog == ']' =  do
    st <- get
    case lookup prog st of
        Just p -> return p
        Nothing -> do
            let progElmts = dropEnd 1 $ drop 1 prog
            let sepElmts = splitOn "," progElmts
            convertedElmts <- mapM matchNiceFunctions sepElmts
            let newProg = printf "[%s]" (intercalate "," convertedElmts)
            modify ((prog, newProg):)
            return newProg
matchNiceFunctions prog | '\\' `elem` prog && "->" `isInfixOf` prog = do
    st <- get
    case lookup prog st of
        Just p -> return p
        Nothing -> do
            let inputs = [-1, 0, 1, 2]
            let concatInputs = [[], [0], [0,0],[1]]
            let concatOutput = [[], [0,0], [0,0,0,0], [1,1]]
            let prog' = if "..." `isInfixOf` prog then replaceId "..." "" prog else prog
            let stmt = printf "GHC.List.map (%s) %s" prog' (show inputs)
            result <- runStmt stmt
            newProg <- case result of
                "[-3,0,3,6]" -> return "\\x -> x * 3"
                "[0,1,2,3]" -> return "\\x -> x + 1"
                "[1,0,1,4]" -> return "\\x -> x * x"
                _ -> do
                    result2 <- runStmt $ printf "GHC.List.map (%s) %s" prog' (show concatInputs)
                    if result2 == show concatOutput
                        then return "\\x -> x ++ x"
                        else do
                            result3 <- runStmt $ printf "(GHC.List.all ((==) (GHC.List.head %s)) %s, GHC.List.head %s)" result result result
                            if take 5 result3 == "(True"
                                then return $ printf "const %s" (dropEnd 1 $ drop 6 result3)
                                else return prog
            modify ((prog, newProg):)
            return newProg
    where
        runStmt p = do
            let mdls = ["Data.Maybe", "GHC.List", "Data.List", "Data.Eq", "GHC.Char", "Data.Function"]
            result <- LHI.runInterpreter $ do
                LHI.setImports mdls
                -- allow extensions for function execution
                extensions <- LHI.get LHI.languageExtensions
                LHI.set [LHI.languageExtensions LHI.:= (LHI.ExtendedDefaultRules : LHI.ScopedTypeVariables : extensions)]
                LHI.eval p
            return $ either show id result
matchNiceFunctions prog = return prog

niceInputs :: Example -> IO Example
niceInputs (Example ins out) = do
    ins' <- evalStateT (mapM matchNiceFunctions ins) []
    return (Example ins' out)
