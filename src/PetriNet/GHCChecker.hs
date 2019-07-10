module PetriNet.GHCChecker (
    runGhcChecks, mkFunctionSigStr, mkLambdaStr,
    removeTypeclasses, toHaskellSolution,
    parseStrictnessSig, checkStrictness', printSolution) where

import Language.Haskell.Interpreter

import Types.Environment
import Types.Program
import Types.Type
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

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

ourFunctionName = "ghcCheckedFunction"

checkStrictness' :: Int -> String -> String -> [String] -> IO Bool
checkStrictness' tyclassCount lambdaExpr typeExpr modules = GHC.runGhc (Just libdir) $ do
    tmpDir <- liftIO $ getTmpDir
    -- TODO: can we use GHC to dynamically compile strings? I think not
    let toModuleImportStr = (printf "import %s\n") :: String -> String
    let moduleImports = concatMap toModuleImportStr modules
    let sourceCode = printf "module Temp where\n%s\n%s :: %s\n%s = %s\n" moduleImports ourFunctionName typeExpr ourFunctionName lambdaExpr
    baseName <- liftIO $ nextRandom
    let baseNameStr = show baseName ++ ".hs"
    let fileName = tmpDir ++ "/" ++ baseNameStr
    liftIO $ writeFile fileName sourceCode

    -- Establishing GHC session
    env <- getSession
    dflags <- getSessionDynFlags
    let dflags' = (updOptLevel 2 dflags)
    setSessionDynFlags $ dflags'

    -- Compile to core
    target <- guessTarget fileName Nothing
    setTargets [target]
    load LoadAllTargets
    modSum <- getModSummary $ mkModuleName "Temp"

    pmod <- parseModule modSum      -- ModuleSummary
    tmod <- typecheckModule pmod    -- TypecheckedSource
    dmod <- desugarModule tmod      -- DesugaredModule
    let core = coreModule dmod      -- CoreModule

    -- Run the demand analyzer
    -- prog is [<fooBinding>, <moduleBinding>]
    core' <- liftIO $ core2core env core
    prog <- liftIO $ (dmdAnalProgram dflags emptyFamInstEnvs $ mg_binds core')
    let decl = findOurBinding (prog :: [CoreBind]) -- only one method
    liftIO $ removeFile fileName
    -- liftIO $ printf "whole program: %s\n" $ showSDocUnsafe $ ppr $ prog
    -- TODO: I'm thinking of simply checking for the presence of `L` (lazy) or `A` (absent)
    -- on the singatures. That would be enough to show that the relevancy requirement is not met.

    case decl of
        NonRec id rest -> do
            -- liftIO $ printf "\nourDecl: %s\n" (showSDocUnsafe $ ppr decl)
            return $ isStrict tyclassCount decl
        _ -> error "checkStrictness: recursive expression found"

    where
        findOurBinding bs = head $ filter (\x-> ourFunctionName `isInfixOf` (showSDocUnsafe $ ppr x)) bs
        getStrictnessSig x = parseStrictnessSig $ showSDocUnsafe $ ppr $ x
        isStrict n x = let
            strictnessSig = getStrictnessSig x
            argStrictness = splitByArg strictnessSig
            restSigs = drop n argStrictness
            in not $ any (elem 'A') restSigs
        splitByArg :: String -> [String]
        splitByArg str = let
            regex = mkRegex "><"
            in splitRegex regex str

parseStrictnessSig :: String -> String
parseStrictnessSig result = let
    regex = mkRegex "Str=(<.*>)"
    in case (matchRegex regex result) of
        Just (match:_) -> match
        _ -> error $ "unable to find strictness in: " ++ result

checkStrictness :: Int -> String -> String -> [String] -> IO Bool
checkStrictness tyclassCount body sig modules = handle (\(SomeException _) -> return False) (checkStrictness' tyclassCount body sig modules)

runGhcChecks :: Bool -> Environment -> RType -> UProgram -> IO Bool
runGhcChecks disableDemand env goalType prog = let
    -- constructs program and its type signature as strings
    args = _arguments env
    modules = Set.toList $ _included_modules env
    argList = Map.toList args
    tyclassCount = length $ Prelude.filter (\(id, _) -> tyclassArgBase `isPrefixOf` id) argList
    argNames = map fst argList
    argTypes = map snd argList
    monoGoals = (map toMonotype argTypes)
    funcSig = mkFunctionSigStr (monoGoals ++ [goalType])
    argTypesMap = Map.fromList $ zip (argNames) (map show monoGoals)
    body = mkLambdaStr argNames prog
    expr = body ++ " :: " ++ funcSig
    in do
        typeCheckResult <- runInterpreter $ checkType expr modules
        strictCheckResult <- if disableDemand then return True else checkStrictness tyclassCount body funcSig modules
        case typeCheckResult of
            Left err -> (putStrLn $ displayException err) >> return False
            Right False -> (putStrLn "Program does not typecheck") >> return False
            Right True -> return strictCheckResult

-- ensures that the program type-checks
checkType :: String -> [String] -> Interpreter Bool
checkType expr modules = do
    setImports ("Prelude":modules)
    -- Ensures that if there's a problem we'll know
    Language.Haskell.Interpreter.typeOf expr
    typeChecks expr

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

toHaskellSolution :: String -> String
toHaskellSolution bodyStr = let
    oneLineBody = unwords $ lines bodyStr
    noTypeclasses = (removeTypeclasses) oneLineBody
    in
        noTypeclasses

printSolution solution = do
    putStrLn "*******************SOLUTION*********************"
    putStrLn $ "SOLUTION: " ++ toHaskellSolution (show solution)
    putStrLn "************************************************"