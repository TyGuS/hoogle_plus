module PetriNet.GHCChecker (runGhcChecks) where

import Language.Haskell.Interpreter

import qualified Data.Set as Set hiding (map)
import Data.Map as Map hiding (map, foldr)
import Control.Exception

import Types.Environment
import Types.Program
import Types.Type
import Synquid.Type
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
import Var
import IdInfo
import Data.Typeable
import Demand
import Data.Data
import FamInstEnv
import DmdAnal
import Data.List (isInfixOf)

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

checkStrictness :: String -> IO Bool
checkStrictness lambdaExpr = GHC.runGhc (Just libdir) $ do

    -- TODO: can we use GHC to dynamically compile strings? I think not
    let sourceCode = "module Temp where\nfoo = " ++ lambdaExpr
    liftIO $ writeFile "Temp.hs" sourceCode

    -- Establishing GHC session
    env <- getSession
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { hscTarget = HscInterpreted }
  
    -- Compile to core
    target <- guessTarget "Temp.hs" Nothing
    setTargets [target]
    load LoadAllTargets
    modSum <- getModSummary $ mkModuleName "Temp"
  
    pmod <- parseModule modSum      -- ModuleSummary
    tmod <- typecheckModule pmod    -- TypecheckedSource
    dmod <- desugarModule tmod      -- DesugaredModule
    let core = coreModule dmod      -- CoreModule
  
    -- Run the demand analyzer
    -- prog is [<fooBinding>, <moduleBinding>]
    prog <- liftIO $ (dmdAnalProgram dflags emptyFamInstEnvs $ mg_binds core)
    let decl = prog !! 0 -- only one method

    -- TODO: I'm thinking of simply checking for the presence of `L` (lazy) or `A` (absent)
    -- on the singatures. That would be enough to show that the relevancy requirement is not met.
    case decl of
        NonRec id _  -> return $ isItStrict id --liftIO $ putStrLn $ getStrictnessSig id
        _ -> error "checkStrictness: recursive expression found"

    where getStrictnessSig x = showSDocUnsafe $ ppr $ strictnessInfo $ idInfo x
          isItStrict x = if isInfixOf "A" (getStrictnessSig x) then False else True

-- TODO: can we remove this?
say :: String -> Interpreter ()
say = liftIO . putStrLn




runGhcChecks :: Environment -> RType -> UProgram -> IO Bool 
runGhcChecks env goalType prog = let
    -- constructs program and its type signature as strings
    args = _arguments env
    modules = Set.toList $ _included_modules env
    argList = Map.toList args
    argNames = map fst argList
    argTypes = map snd argList
    funcSig = mkFunctionSigStr (map toMonotype argTypes) goalType
    body = mkLambdaStr argNames prog
    expr = body ++ " :: " ++ funcSig

    -- ensures that the program type-checks
    checkType :: Interpreter Bool
    checkType = do
        setImports ("Prelude":modules)
        -- Ensures that if there's a problem we'll know
        Language.Haskell.Interpreter.typeOf expr
        typeChecks expr

    in do
        typeCheckResult <- runInterpreter checkType
        strictCheckResult <- checkStrictness body 
        case typeCheckResult of
            Left err -> (putStrLn $ displayException err) >> return False
            Right False -> (putStrLn "Program does not typecheck") >> return False
            Right True -> return strictCheckResult

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
