{-# LANGUAGE TupleSections #-}

module HooglePlus.GHCChecker (
    runGhcChecks, parseStrictnessSig, checkStrictness', check) where

import Language.Haskell.Interpreter hiding (get)

import Types.Environment
import Types.Program
import Types.Type
import Types.Experiments
import Types.Filtering
import Types.IOFormat
import Synquid.Type
import Synquid.Util hiding (fromRight)
import Synquid.Pretty as Pretty
import Database.Util
import HooglePlus.Utils
import HooglePlus.FilterTest (runChecks)
import HooglePlus.IOFormat
import Examples.ExampleChecker

import Control.Exception
import Control.Monad.Trans
import CorePrep
import CoreSyn
import Data.Data
import Data.Either
import Data.List (nubBy, isInfixOf, isPrefixOf, intercalate)
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
import Control.Concurrent.Chan
import Control.Monad.Trans.State
import Control.Concurrent
import Debug.Trace

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

ourFunctionName = "ghcCheckedFunction"

checkStrictness' :: Int -> String -> String -> [String] -> IO Bool
checkStrictness' tyclassCount lambdaExpr typeExpr modules = GHC.runGhc (Just libdir) $ do
    tmpDir <- liftIO getTmpDir
    -- TODO: can we use GHC to dynamically compile strings? I think not
    let toModuleImportStr = printf "import %s\n" :: String -> String
    let moduleImports = concatMap toModuleImportStr modules
    let sourceCode = printf "module Temp where\n%s\n%s :: %s\n%s = %s\n" moduleImports ourFunctionName typeExpr ourFunctionName lambdaExpr
    baseName <- liftIO nextRandom
    let baseNameStr = show baseName
    let fileName = tmpDir ++ "/" ++ baseNameStr
    let fileNameHs = fileName ++ ".hs"
    liftIO $ writeFile fileNameHs sourceCode

    -- Establishing GHC session
    env <- getSession
    dflags <- getSessionDynFlags
    let dflags' = (updOptLevel 2 dflags)
    setSessionDynFlags dflags'

    -- Compile to core
    target <- guessTarget fileNameHs Nothing
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
    prog <- liftIO $ dmdAnalProgram dflags emptyFamInstEnvs $ mg_binds core'
    let decl = findOurBinding (prog :: [CoreBind]) -- only one method
    liftIO $ removeFile fileNameHs
    liftIO $ removeFile (fileName ++ ".hi")
    liftIO $ removeFile (fileName ++ ".o")
    -- liftIO $ printf "whole program: %s\n" $ showSDocUnsafe $ ppr $ prog
    -- TODO: I'm thinking of simply checking for the presence of `L` (lazy) or `A` (absent)
    -- on the singatures. That would be enough to show that the relevancy requirement is not met.

    case decl of
        NonRec id rest -> return $ isStrict tyclassCount decl
        _ -> error "checkStrictness: recursive expression found"

    where
        findOurBinding bs = head $ filter (\x-> ourFunctionName `isInfixOf` showSDocUnsafe (ppr x)) bs
        getStrictnessSig x = parseStrictnessSig $ showSDocUnsafe $ ppr x
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
checkStrictness tyclassCount body sig modules =
    handle
        (\(SomeException _) -> return False)
        (checkStrictness' tyclassCount body sig modules)

check :: MonadIO m 
       => Environment -- symbol environment
       -> SearchParams -- search parameters: to control what to be checked
       -> [Example] -- examples for post-filtering
       -> RProgram -- program to be checked
       -> RSchema -- goal type to be checked against
       -> Chan Message -- message channel for logging
       -> FilterTest m (Maybe AssociativeExamples) -- return Nothing is check fails, otherwise return a list of updated examples
check env searchParams examples program goalType solverChan =
    runGhcChecks searchParams env (lastType $ toMonotype goalType) examples program

-- validate type signiture, run demand analysis, and run filter test
-- checks the end result type checks; all arguments are used; and that the program will not immediately fail
runGhcChecks :: MonadIO m 
             => SearchParams 
             -> Environment 
             -> RType 
             -> [Example]
             -> UProgram 
             -> FilterTest m (Maybe AssociativeExamples)
runGhcChecks params env goalType examples prog = let
    -- constructs program and its type signature as strings
    tyclassCount = length $ Prelude.filter (\(id, _) -> tyclassArgBase `isPrefixOf` id) argList
    expr = printf "(%s) :: %s" body funcSig
    disableDemand = _disableDemand params
    disableFilter = _disableFilter params
    in do
        typeCheckResult <- liftIO $ runInterpreter $ checkType expr modules
        strictCheckResult <- if disableDemand then return True else liftIO $ checkStrictness tyclassCount body funcSig modules
        exampleCheckResult <- if not strictCheckResult then return Nothing else liftIO $ fmap ((:[]) . (body,)) <$> checkOutputs prog examples
        filterCheckResult <- if disableFilter || isNothing exampleCheckResult
                                then return exampleCheckResult
                                else do
                                    filterResult <- runChecks env goalType prog
                                    if isNothing filterResult
                                       then return filterResult
                                       else return $ Just (fromJust exampleCheckResult ++ fromJust filterResult)
        case typeCheckResult of
            Left err -> liftIO $ putStrLn (displayException err) >> return Nothing
            Right False -> liftIO $ putStrLn "Program does not typecheck" >> return Nothing
            Right True -> return filterCheckResult
    where
        mdls = Set.toList (_included_modules env)
        (modules, funcSig, body, argList) = extractSolution env goalType prog
        checkOutputs prog exs = checkExampleOutput mdls env funcSig (show prog) exs

-- ensures that the program type-checks
checkType :: String -> [String] -> Interpreter Bool
checkType expr modules = do
    setImports modules
    -- Ensures that if there's a problem we'll know
    Language.Haskell.Interpreter.typeOf expr
    typeChecks expr
