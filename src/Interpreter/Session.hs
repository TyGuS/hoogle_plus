module Interpreter.Session (
  -- * Session
    InterpreterSession(..)
  , InterpreterState(..)
  , InterpreterError(..)
  , Sessions(..)
  , initialState
  , newInterpreterSession

  -- * Configuration
  , InterpreterConfig(..)
  , defaultConfig

  -- * Imports
  , ImportList(..)
  , ModuleQualification(..)
  , ModuleName
  , ModuleImport(..)
  , isPhantomModule
  , isQualified
  , hasImportList
) where

import Data.List ( nub )
import Data.Typeable
import Data.IORef
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified GHC
import qualified GhcMonad as GHC
import GHC.LanguageExtensions.Type

----------------------------------------------------
---------------  Session and State -----------------
----------------------------------------------------

data InterpreterSession = InterpreterSession {
  interpreterState :: IORef InterpreterState,
  ghcErrorList     :: IORef [String]
}

data ImportList = NoImportList | ImportList [String] | HidingList [String]
  deriving (Eq, Show)

data ModuleQualification = NotQualified | ImportAs String | QualifiedAs (Maybe String)
  deriving (Eq, Show)

type ModuleName = String

data ModuleImport = ModuleImport { modName :: String
                                 , modQual :: ModuleQualification
                                 , modImp  :: ImportList
                                 } deriving (Show)

isQualified :: ModuleImport -> Bool
isQualified (ModuleImport _ NotQualified _) = False
isQualified _ = True

hasImportList :: ModuleImport -> Bool
hasImportList (ModuleImport _ _ NoImportList) = False
hasImportList _ = True

isPhantomModule :: ModuleImport -> Bool
isPhantomModule m = isQualified m || hasImportList m

newInterpreterSession :: MonadIO m => m InterpreterSession
newInterpreterSession = do
  st <- liftIO $ newIORef initialState
  errLst <- liftIO $ newIORef []
  return InterpreterSession {
    interpreterState = st,
    ghcErrorList = errLst
  }

data Sessions = Sessions {
  gSession :: GHC.Session,
  iSession :: InterpreterSession
}

data InterpreterState = InterpreterState {
  qualImports   :: [ModuleImport],
  activeExts    :: [Extension],
  configuration :: InterpreterConfig
}

initialState :: InterpreterState
initialState = InterpreterState {
  qualImports = [],
  activeExts  = [],
  configuration = defaultConfig
}

data InterpreterError = WontCompile [String]
                      | GhcException String
                      | UnknownError String
  deriving (Eq, Show, Typeable)

instance Exception InterpreterError
  where
    displayException (UnknownError err) = "UnknownError: " ++ err
    displayException (WontCompile  es)  = unlines (nub es)
    displayException (GhcException err) = "GhcException: " ++ err

-----------------------------------------------------
------------------- Configuration -------------------
-----------------------------------------------------

data InterpreterConfig = InterpreterConfig {
  languageExts :: [Extension],
  searchFilePath :: [FilePath]
}

defaultConfig :: InterpreterConfig
defaultConfig = InterpreterConfig {
  languageExts = [],
  searchFilePath = ["."]
}

