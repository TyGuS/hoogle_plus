module PetriNet.GHCChecker (
    runGhcChecks, mkFunctionSigStr, mkLambdaStr,
    removeTypeclassInstances, toHaskellSolution) where

import Language.Haskell.Interpreter

import qualified Data.Set as Set hiding (map)
import qualified Data.Map as Map hiding (map, foldr)
import Control.Exception
import Data.Maybe

import Types.Environment
import Types.Program
import Types.Type
import Synquid.Type
import Synquid.Util hiding (fromRight)
import Synquid.Pretty as Pretty
import Database.Util

-- TODO: filter out unecessary imports
import Data.List.Split (splitOn)
import GHC
import GHC.Paths ( libdir )
import HscTypes
import CorePrep
import DynFlags
import CoreSyn
import Outputable hiding (text, (<+>))
import qualified CoreSyn as Syn
import Control.Monad.Trans
import Control.Exception
import Var
import IdInfo
import Data.Typeable
import Data.Either
import Demand
import Data.Data
import FamInstEnv
import DmdAnal
import Data.List (isInfixOf, isPrefixOf)
import System.Directory (removeFile)
import Text.Printf
import SimplCore (core2core)
import Text.Regex
import Data.List (intercalate)
import qualified Data.Text as Text
import Data.Maybe (fromJust)

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
        NonRec id _  -> do
            return $ isStrict tyclassCount id --liftIO $ putStrLn $ getStrictnessSig id
        _ -> error "checkStrictness: recursive expression found"

    where 
        getStrictnessSig x = showSDocUnsafe $ ppr $ strictnessInfo $ idInfo x
        isStrict n x = let
            strictnessSig = getStrictnessSig x
            argStrictness = splitByArg strictnessSig
            typeclassSigs = take n argStrictness
            restSigs = drop n argStrictness
            allTypeclassesUsed = all isTypeclassUsed typeclassSigs
            restSigsUsed = all (not . elem 'A') restSigs
            in allTypeclassesUsed && restSigsUsed
        splitByArg :: String -> [String]
        splitByArg str = let
            regex = mkRegex "<[^>]*>"
            matches = matchRegex regex str
            in fromMaybe [] matches
        -- Ensure we use SOME part of the typeclass
        isTypeclassUsed :: String -> Bool
        isTypeclassUsed str = not $ all ('A' `elem`) $ splitOn "," str

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
    in
        unwords . words $ subRegex regex x ""

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