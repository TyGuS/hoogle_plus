module Types.Generate (
    GenerationOpts(..)
  , PackageFetchOpts(..)
  , Entry(..)
  , HigherOrderOption(..)
  , Preset(..)
  , PkgName
  , MdlName
  , defaultLocalOpts
  , defaultGenerationOpts
  , defaultEnvPath
  , defaultHoPath
  , defaultHooglePath
  , defaultJsonPath
  ) where

import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           Data.Map                       ( Map )
import           Language.Haskell.Exts          ( Decl )
import qualified Language.Haskell.Exts.Syntax  as HSE

type Version = String
type PkgName = String
type MdlName = String

data GenerationOpts = GenerationOpts
  { instantiationDepth :: Int
  , enableHOF          :: Bool
  , pkgFetchOpts       :: PackageFetchOpts
  , modules            :: [String]
  , envPath            :: FilePath
  , hoPath             :: FilePath
  , hooglePath         :: FilePath
  , hoOption           :: HigherOrderOption
  }
  deriving (Show, Typeable, Eq)

newtype PackageFetchOpts
    = Local {
        files :: [String]
        }
    deriving (Show, Typeable, Eq)

data Entry
    = EPackage String
    | EModule String
    | EDecl HDeclaration
    deriving (Data, Typeable, Show, Eq, Ord)

data HigherOrderOption
    = HOFAll
    | HOFPartial
    deriving (Eq, Ord, Show, Data, Typeable)

type HDeclaration = HSE.Decl ()

data Preset
    = TotalFunctions
    | PartialFunctions
    | ECTAFull
    | ECTAPartial
    deriving (Eq, Show, Data, Typeable)

defaultLocalOpts :: PackageFetchOpts
defaultLocalOpts = Local { files = ["working/newbase.txt"] }

defaultGenerationOpts :: GenerationOpts
defaultGenerationOpts = GenerationOpts { instantiationDepth = 0
                                       , enableHOF          = True
                                       , pkgFetchOpts       = defaultLocalOpts
                                       , modules            = []
                                       , envPath            = defaultEnvPath
                                       , hoPath             = defaultHoPath
                                       , hooglePath         = defaultHooglePath
                                       , hoOption           = HOFPartial
                                       }

defaultEnvPath :: FilePath
defaultEnvPath = "data/env.db"

defaultHoPath :: FilePath
defaultHoPath = "data/ho.txt"

defaultHooglePath :: FilePath
defaultHooglePath = "data/hoogle.db"

defaultJsonPath :: FilePath
defaultJsonPath = "data/builtin.json"
