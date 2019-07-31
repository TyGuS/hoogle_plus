module HooglePlus.Utils where

import Types.Environment
import Types.Program
import Types.Type
import Types.Experiments
import Synquid.Type
import Synquid.Util hiding (fromRight)
import Synquid.Pretty as Pretty
import Database.Util

import Control.Exception
import Control.Monad.Trans
import CorePrep
import CoreSyn
import Data.Data
import Data.Either
import Data.List (isInfixOf, isPrefixOf, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Typeable
import Demand
import DmdAnal
import DynFlags
import FamInstEnv
import GHC
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
import Var
import Data.UUID.V4

-- Converts the list of param types into a haskell function signature.
-- Moves typeclass-looking things to the front in a context.
mkFunctionSigStr :: [RType] -> String
mkFunctionSigStr args = addConstraints $ Prelude.foldr accumConstraints ([],[]) args
    where
        showSigs sigs = intercalate " -> " sigs
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
        accumConstraints otherTy (constraints, baseSigs) = (constraints, show otherTy:baseSigs)

-- mkLambdaStr produces a oneline lambda expr str:
-- (\x -> \y -> body))
mkLambdaStr :: [String] -> UProgram -> String
mkLambdaStr args body = let
    unTypeclassed = toHaskellSolution (show body)
    in
        unwords . words . show $ foldr addFuncArg (text unTypeclassed) args
    where
        addFuncArg arg rest
            | "arg" `isPrefixOf` arg = Pretty.parens $ text ("\\" ++ arg ++ " -> ") <+> rest
            | otherwise = rest

toHaskellSolution :: String -> String
toHaskellSolution bodyStr = let
    oneLineBody = unwords $ lines bodyStr
    noTypeclasses = (removeTypeclasses) oneLineBody
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
removeTypeclassArgs = removeAll (mkRegex (tyclassArgBase++"[0-9]+\\s?"))

removeTypeclassInstances :: String -> String
removeTypeclassInstances = removeAll (mkRegex (tyclassInstancePrefix ++ "[0-9]*[a-zA-Z]*"))

removeTypeclasses = removeEmptyParens . removeTypeclassArgs . removeTypeclassInstances
    where
        removeEmptyParens = removeAll (mkRegex "\\(\\s+\\)")

printSolution solution = do
    putStrLn "*******************SOLUTION*********************"
    putStrLn $ "SOLUTION: " ++ toHaskellSolution (show solution)
    putStrLn "************************************************"