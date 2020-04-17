{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Examples.Utils where

import GHC hiding (Id)
import GHC.Paths
import qualified EnumSet as ES
import GHC.LanguageExtensions.Type
import HsUtils
import HsTypes
import TcRnDriver
import Exception
import Debugger
import qualified Language.Haskell.Interpreter as LHI
import System.Timeout
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Exception
import Data.Char
import Data.List
import Data.List.Extra (dropEnd)
import Outputable
import Control.Monad.State
import Control.Lens
import Control.Concurrent.Chan

import Types.Filtering (defaultTimeoutMicro, defaultDepth, defaultInterpreterTimeoutMicro, frameworkModules)
import Types.IOFormat
import Types.Type
import Types.Common
import Types.Environment
import Types.Experiments
import Types.TypeChecker
import Types.InfConstraint
import Database.Util
import Synquid.Logic
import Synquid.Type
import HooglePlus.FilterTest (runInterpreter')
import HooglePlus.TypeChecker (solveTypeConstraint)
import PetriNet.Util

askGhc :: [String] -> Ghc a -> IO a
askGhc mdls f = do
    mbResult <- timeout (10^6) $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = dflags { 
            generalFlags = ES.delete Opt_OmitYields (generalFlags dflags),
            extensionFlags = ES.insert FlexibleContexts (extensionFlags dflags)
            }
        setSessionDynFlags dflags'
        prepareModules ("Test.ChasingBottoms":"Prelude":mdls) >>= setContext
        f
    case mbResult of
        Just r -> return r
        Nothing -> error "timeout"
    where
        prepareModules mdls = do
            let imports = map (printf "import %s") mdls
            decls <- mapM parseImportDecl imports
            return (map IIDecl decls)

runStmt :: [String] -> String -> IO (Either ErrorMessage String)
runStmt mdls prog = do
  catch (askGhc mdls $ do
    -- allow type defaulting during execution
    dflags <- getSessionDynFlags
    let dflags' = dflags {
        extensionFlags = ES.insert ExtendedDefaultRules (extensionFlags dflags)
        }
    setSessionDynFlags dflags'
    result <- execStmt prog execOptions
    case result of
        ExecComplete r _ -> case r of
                            Left e -> return (Left (show e))
                            Right ns -> getExecValue ns
        ExecBreak {} -> return (Left "error, break"))
    (\(e :: SomeException) -> return (Left $ show e))
    where
        getExecValue (n:ns) = do
            mty <- lookupName n
            case mty of
                Just (AnId aid) -> do
                    t <- gtry $ obtainTermFromId maxBound True aid
                    case t of
                        Right term -> showTerm term >>= return . Right . dropEnd 1 . drop 1 . showSDocUnsafe
                        Left (exn :: SomeException) -> return (Left $ show exn)
                _ -> return (Left "Unknown error")
        getExecValue [] = return (Left "Empty result list")

skipTyclass :: TypeSkeleton r -> TypeSkeleton r
skipTyclass (FunctionT x (ScalarT (DatatypeT name args _) _) tRes)
    | tyclassPrefix `isPrefixOf` name = skipTyclass tRes
skipTyclass t = t

seqChars = map (:[]) ['a'..'z']

integerToInt :: TypeSkeleton r -> TypeSkeleton r
integerToInt (ScalarT (DatatypeT dt args _) r) 
  | dt == "Integer" = ScalarT (DatatypeT "Int" (map integerToInt args) []) r
  | otherwise = ScalarT (DatatypeT dt (map integerToInt args) []) r 
integerToInt (FunctionT x tArg tRes) =
    FunctionT x (integerToInt tArg) (integerToInt tRes)
integerToInt t = t

wrapParens :: String -> String
wrapParens = printf "(%s)"

supportedTyclasses :: [String]
supportedTyclasses = ["Num", "Ord", "Eq"]

checkTypes :: Environment -> Chan Message -> RSchema -> RSchema -> IO (Bool, SType)
checkTypes env checkerChan s1 s2 = do
    let initChecker = emptyChecker { _checkerChan = checkerChan }
    let bound = env ^. boundTypeVars
    (t, state) <- runStateT (do
        r1 <- freshType bound s1
        r2 <- freshType bound s2
        let t1 = skipTyclass r1
        let t2 = skipTyclass r2
        solveTypeConstraint env (shape t1) (shape t2)
        tass <- gets $ view typeAssignment
        return $ stypeSubstitute tass $ shape r2) initChecker
    return (state ^. isChecked, t)

mkPolyType :: TypeSkeleton r -> SchemaSkeleton r
mkPolyType t = let tvars = Set.toList $ typeVarsOf t
                   freeVars = filter ((==) univTypeVarPrefix . head) tvars
                in foldr ForallT (Monotype t) freeVars
