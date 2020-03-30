module HooglePlus.Utils where

import Types.Common
import Types.Environment
import Types.Program
import Types.Type
import Types.Experiments
import Types.Filtering
import Synquid.Type
import Synquid.Util hiding (fromRight)
import Synquid.Pretty as Pretty
import Synquid.Program
import Database.Util

import Control.Exception
import Control.Monad.Trans
import CorePrep
import CoreSyn
import Data.Data
import Data.Ord
import Data.Either
import Data.List (nub, sortOn, groupBy, isInfixOf, isPrefixOf, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Typeable
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

-- Converts the list of param types into a haskell function signature.
-- Moves typeclass-looking things to the front in a context.
mkFunctionSigStr :: [RType] -> String
mkFunctionSigStr args = addConstraints $ Prelude.foldr accumConstraints ([],[]) args
    where
        showSigs = intercalate " -> "
        wrapParen x = "(" ++ x ++ ")"
        addConstraints ([], baseSigs) = showSigs baseSigs
        addConstraints (constraints, baseSigs) = "(" ++ (intercalate ", " constraints) ++ ") => " ++ showSigs baseSigs

        accumConstraints :: RType -> ([String], [String]) -> ([String], [String])
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
-- (\x -> \y -> body))
mkLambdaStr :: [String] -> UProgram -> String
mkLambdaStr args body = let
    unTypeclassed = toHaskellSolution (show body)
    in
        unwords . words . show $ foldr addFuncArg (text unTypeclassed) args
    where
        addFuncArg arg rest
            | arg `elem` args && not (tyclassArgBase `isPrefixOf` arg) =
                Pretty.parens $ text ("\\" ++ arg ++ " -> ") <+> rest
            | otherwise = rest

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

{-
printFilter (FilterState _ solns samples) = unlines $ map printSol solns
    where
        printSol :: String -> String
        printSol sol =
            let [(_, desc)] = filter ((== sol) . fst) samples in
                unlines [sol, show desc]
-}

printSolutionState solution (FilterState _ sols samples diffExamples) = unlines [ios, diffs]
    where
        ios = let [(_, desc)] = filter ((== solution) . fst) samples in show desc
        diffs = unlines $ map showDifferentiations diffExamples
        
        showDifferentiations :: DiffInstance -> String
        showDifferentiations (args, outs) = unlines ((unwords args) : zipWith combineSolutionOutput sols outs)

        combineSolutionOutput sol out = printf "%s ==> %s" sol out :: String

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
