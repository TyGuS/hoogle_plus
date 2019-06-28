module PetriNet.GHCChecker (
    runGhcChecks, mkFunctionSigStr, mkLambdaStr,
    removeTypeclassInstances, toHaskellSolution,
    parseStrictnessSig, checkStrictness') where

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

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

checkStrictness' :: Int -> String -> String -> [String] -> IO Bool
checkStrictness' tyclassCount lambdaExpr typeExpr modules = GHC.runGhc (Just libdir) $ do
    tmpDir <- liftIO $ getTmpDir
    -- TODO: can we use GHC to dynamically compile strings? I think not
    let toModuleImportStr = (printf "import %s\n") :: String -> String
    let moduleImports = concatMap toModuleImportStr modules
    let sourceCode = printf "module Temp where\n%s\nfoo :: %s\nfoo = %s\n" moduleImports typeExpr lambdaExpr
    let fileName = tmpDir ++ "/Temp.hs"
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
    let decl = prog !! 0 -- only one method
    -- liftIO $ removeFile fileName

    -- TODO: I'm thinking of simply checking for the presence of `L` (lazy) or `A` (absent)
    -- on the singatures. That would be enough to show that the relevancy requirement is not met.
    case decl of
        NonRec id rest -> do
            return $ isStrict tyclassCount decl --liftIO $ putStrLn $ getStrictnessSig id
        _ -> error "checkStrictness: recursive expression found"

    where
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
    unTypeclassed = toHaskellSolution body
    in
        unwords . words . show $ foldr addFuncArg (text unTypeclassed) args
    where
        addFuncArg arg rest
            | "arg" `isPrefixOf` arg = Pretty.parens $ text ("\\" ++ arg ++ " -> ") <+> rest
            | otherwise = rest

removeTypeclassInstances :: String -> String
removeTypeclassInstances x = let
    regex = mkRegex $ "\\(" ++ tyclassInstancePrefix ++ "[0-9]*@@[a-zA-Z]* ("++tyclassArgBase++"[0-9]+\\s?)*\\)"
    containsMatch = isJust $ matchRegex regex x
    in
        if containsMatch
            then  removeTypeclassInstances $ unwords . words $ subRegex regex x ""
            else x

toHaskellSolution :: UProgram -> String
toHaskellSolution body = let
    bodyStr = show body
    oneLineBody = unwords $ lines bodyStr
    noInstances = removeTypeclassInstances oneLineBody
    unTypeclassed = removeTcArgs noInstances
    in
        unwords $ words $ unTypeclassed
    where
        removeTcArgs str = let
            regex = mkRegex $ tyclassArgBase++"[0-9]+\\s?"
            in subRegex regex str ""