module Examples.Utils where

import Data.Functor ((<&>))
import Data.List.Extra (dropEnd)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Debugger (showTerm)
import qualified EnumSet as ES
import Exception (SomeException, catch, gtry)
import GHC hiding (Id)
import GHC.LanguageExtensions.Type (Extension(ExtendedDefaultRules, FlexibleContexts))
import GHC.Paths (libdir)
import qualified Language.Haskell.Interpreter as LHI
import Outputable (showSDocUnsafe)
import System.Timeout (timeout)
import Text.Printf (printf)

import Types.Common
import Types.Environment
import Types.Filtering (defaultDepth, defaultInterpreterTimeoutMicro, defaultTimeoutMicro)
import Types.Substitution
import Types.Type
import Types.TypeChecker

askGhc :: [Id] -> Ghc a -> IO a
askGhc mdls f = do
  mbResult <- timeout (10 ^ 6) $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = dflags
          { generalFlags   = ES.delete Opt_OmitYields (generalFlags dflags), extensionFlags = ES.insert FlexibleContexts (extensionFlags dflags)
          }
    setSessionDynFlags dflags'
    prepareModules ("Test.ChasingBottoms" : "Prelude" : mdls) >>= setContext
    f
  case mbResult of
    Just r  -> return r
    Nothing -> error "timeout"
 where
  prepareModules mdls = do
    let imports = map (printf "import %s") mdls
    decls <- mapM parseImportDecl imports
    return (map IIDecl decls)

skipTyclass :: TypeSkeleton -> TypeSkeleton
skipTyclass (FunctionT x (DatatypeT name args) tRes)
  | tyclassPrefix `Text.isPrefixOf` name = skipTyclass tRes
skipTyclass t = t

seqChars :: [Id]
seqChars = map Text.singleton ['a' .. 'z']

integerToInt :: TypeSkeleton -> TypeSkeleton
integerToInt (DatatypeT dt args)
  | dt == "Integer" = DatatypeT "Int" (map integerToInt args)
  | otherwise       = DatatypeT dt (map integerToInt args)
integerToInt (FunctionT x tArg tRes) =
  FunctionT x (integerToInt tArg) (integerToInt tRes)
integerToInt t = t

wrapParens :: String -> String
wrapParens = printf " (%s)"