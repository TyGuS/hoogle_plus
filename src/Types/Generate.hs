{-# LANGUAGE DeriveDataTypeable #-}
module Types.Generate where

import Types.Common

import Language.Haskell.Exts (Decl)
import Data.Map.Strict (Map)
import qualified Language.Haskell.Exts.Syntax as HSE
import Data.Data
import Data.Typeable

type Version = String
type PkgName = String
type MdlName = String

data GenerationOpts = GenerationOpts {
    instantiationDepth :: Int,
    enableHOF :: Bool,
    pkgFetchOpts :: PackageFetchOpts,
    modules :: [String],
    envPath :: FilePath,
    hoPath :: FilePath,
    hooglePath :: FilePath
    }
    deriving (Show, Typeable, Eq)

data PackageFetchOpts
    = Hackage {
        packages :: [String]
        }
    | Local {
        files :: [String]
        }
    deriving (Show, Typeable, Eq)

data Entry
    = EPackage String
    | EModule String
    | EDecl HDeclaration
    deriving (Data, Typeable, Show, Eq, Ord)

type DependsOn = Map PkgName [Id]
type HType = HSE.Type ()
type HName = HSE.Name ()
type HExp = HSE.Exp ()
type HDeclaration = HSE.Decl ()

data Preset = TotalFunctions | PartialFunctions deriving (Eq, Show, Data, Typeable)

defaultHackageOpts = Hackage {
    packages = ["base"]
    }

defaultLocalOpts = Local {
    files = ["working/newbase.txt"]
    }

defaultGenerationOpts = GenerationOpts {
    instantiationDepth = 0,
    enableHOF = True,
    pkgFetchOpts = defaultLocalOpts,
    modules = [],
    envPath = defaultEnvPath,
    hoPath = defaultHoPath,
    hooglePath = defaultHooglePath
    }

defaultEnvPath = "data/env.db"
defaultHoPath = "data/ho.txt"
defaultHooglePath = "data/hoogle.db"
defaultJsonPath = "data/builtin.json"
