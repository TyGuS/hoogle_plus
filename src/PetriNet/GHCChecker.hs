module PetriNet.GHCChecker (runGhcChecks) where

import Language.Haskell.Interpreter

import qualified Data.Set as Set hiding (map)
import Data.Map as Map hiding (map, foldr)
import Control.Exception

import Types.Environment
import Types.Program
import Types.Type
import Synquid.Type
import Synquid.Util hiding (fromRight)
import Synquid.Pretty as Pretty

-- TODO: filter out unecessary imports
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
import Data.List (isInfixOf)
import System.Directory (removeFile)
import Text.Printf
import SimplCore (core2core)
import Text.Regex.PCRE
import Data.List (intercalate)
import qualified Data.Text as Text
import Data.Maybe (fromJust)

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

checkStrictness' :: String -> [String] -> IO Bool
checkStrictness' lambdaExpr modules = GHC.runGhc (Just libdir) $ do
    tmpDir <- liftIO $ getTmpDir
    -- TODO: can we use GHC to dynamically compile strings? I think not
    let toModuleImportStr = (printf "import qualified %s\n") :: String -> String
    let moduleImports = concatMap toModuleImportStr modules
    let sourceCode = printf "module Temp where\n%s\nfoo = %s\n" moduleImports lambdaExpr
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
    liftIO $ removeFile fileName

    -- TODO: I'm thinking of simply checking for the presence of `L` (lazy) or `A` (absent)
    -- on the singatures. That would be enough to show that the relevancy requirement is not met.
    case decl of
        NonRec id _  -> do return $ isStrict id --liftIO $ putStrLn $ getStrictnessSig id
        _ -> error "checkStrictness: recursive expression found"

    where getStrictnessSig x = showSDocUnsafe $ ppr $ strictnessInfo $ idInfo x
          isStrict x = not(isInfixOf "A" (getStrictnessSig x))

checkStrictness :: String -> [String] -> IO Bool
checkStrictness a b = handle (\(SomeException _) -> return False) (checkStrictness' a b)

runGhcChecks :: Bool -> Environment -> RType -> UProgram -> IO Bool
runGhcChecks disableDemand env goalType prog = let
    -- constructs program and its type signature as strings
    args = _arguments env
    modules = Set.toList $ _included_modules env
    argList = Map.toList args
    argNames = map fst argList
    argTypes = map snd argList
    monoGoals = (map toMonotype argTypes) 
    funcSig = mkFunctionSigStr monoGoals goalType 
    argTypesMap = fromList $ zip (argNames) (map show monoGoals)
    body = mkLambdaStr argNames prog
    expr = body ++ " :: " ++ funcSig
    in do
        print prog
        print expr
        print  argList
        let b = foo body env argTypesMap
        print b
        typeCheckResult <- runInterpreter $ checkType expr modules
        strictCheckResult <- if disableDemand then return True else checkStrictness body modules
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

-- mkFunctionSigStr generates a function's type signature:
-- Int -> Data.Foo.Foo -> Bar
mkFunctionSigStr :: [RType] -> RType -> String
mkFunctionSigStr [] tyRet = show tyRet
mkFunctionSigStr (argTy:argTys) tyRet
    = show argTy ++ " -> " ++ mkFunctionSigStr argTys tyRet

-- mkLambdaStr produces a oneline lambda expr str:
-- (\x -> \y -> body))
mkLambdaStr :: [String] -> UProgram -> String
mkLambdaStr args body = let
    bodyStr = show body
    oneLineBody = unwords $ lines bodyStr
    addFuncArg arg rest = Pretty.parens $ text ("\\" ++ arg ++ " -> ") <+> rest
    in
        show $ foldr addFuncArg (text oneLineBody) args

-- TODO: if list is empty, then return empty!!
foo :: String -> Environment -> Map String String -> String
foo x env typesMap = let
    stringResults = "__hplusTCTransition[0-9]*__[a-ZA-Z]* arg[0-9]+" =~ x :: AllTextMatches [] String
    matches = (getAllTextMatches stringResults)
    bar = map (\x -> x) matches
    typeclassesStr =  "(" ++ (intercalate ", " bar) ++ ")" ++ " => "
    in if (length bar) == 0 then "" else typeclassesStr
    where
        -- TODO: just do a map lookup!
        argNameToType name typesMap = fromJust $ Map.lookup name typesMap
        funNameToTypeclass :: String -> String
        funNameToTypeclass name = Text.unpack $ (reverse $ (Text.splitOn (Text.pack "__") (Text.pack name))) !! 0
        mapMe x = 
            let (funName, argName) = break (== ' ') x
                argType = argNameToType argName typesMap
                typeclass = funNameToTypeclass funName
                in typeclass ++ " " ++ argType