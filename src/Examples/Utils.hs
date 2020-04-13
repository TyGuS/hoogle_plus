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
import qualified Language.Haskell.Interpreter as LHI
import System.Timeout
import Text.Printf
import qualified Data.Map as Map
import Control.Exception
import Data.Char
import Data.List
import Outputable
import Control.Monad.IO.Class

import Types.Filtering (defaultTimeoutMicro, defaultDepth, defaultInterpreterTimeoutMicro, frameworkModules)
import Types.IOFormat
import Types.Type
import Types.Common
import Database.Util
import Synquid.Logic
import HooglePlus.FilterTest (runInterpreter')

askInterpreter :: [String] -> String -> String -> IO (Either ErrorMessage String)
askInterpreter mdls preamble funcCall = do
    let progCall = printf "%s showCBResult <$> (CB.timeOutMicro' %d (CB.approxShow %d (%s)))" preamble defaultTimeoutMicro defaultDepth funcCall 
    catch (do
        result <- runInterpreter' defaultInterpreterTimeoutMicro $ do
            LHI.setImportsQ (zip mdls (repeat Nothing) ++ frameworkModules)
            r <- LHI.interpret progCall (LHI.as :: IO String) >>= liftIO
            return r
        print result
        case result of
          Left e -> return (Left $ show e)
          Right r -> return (Right r))
        (\(e :: SomeException) -> return (Left $ show e))

askGhc :: [String] -> Ghc a -> IO a
askGhc mdls f = do
    mbResult <- timeout (5*defaultTimeoutMicro) $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = dflags { 
            generalFlags = ES.delete Opt_OmitYields (generalFlags dflags),
            extensionFlags = ES.insert FlexibleContexts (extensionFlags dflags)
            }
        setSessionDynFlags dflags'
        prepareModules ("System.Timeout":"Prelude":mdls) >>= setContext
        f
    case mbResult of
        Just r -> return r
        Nothing -> error "timeout"
    where
        prepareModules mdls = do
            let imports = map (printf "import %s") mdls
            decls <- mapM parseImportDecl imports
            return (map IIDecl decls)

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
