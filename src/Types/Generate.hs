{-# LANGUAGE DeriveDataTypeable #-}

module Types.Generate where

import Types.Common

import Data.Data
import Data.Map (Map)
import Data.Typeable
import Language.Haskell.Exts (Decl)
import qualified Language.Haskell.Exts.Syntax as HSE

type Version = String

type PkgName = String

type MdlName = String

data GenerationOpts =
    GenerationOpts
        { instantiationDepth :: Int
        , enableHOF :: Bool
        , pkgFetchOpts :: PackageFetchOpts
        , modules :: [String]
        , envPath :: FilePath
        , hoPath :: FilePath
        }
    deriving (Show, Typeable, Eq)

data PackageFetchOpts
    = Hackage
          { packages :: [String]
          }
    | Local
          { files :: [String]
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

data Preset
    = ICFPTotal
    | ICFPPartial
    | POPL
    deriving (Eq, Show, Data, Typeable)

defaultHackageOpts = Hackage {packages = []}

defaultLocalOpts = Local {files = ["working/newbase.txt"]}

defaultGenerationOpts =
    GenerationOpts
        { instantiationDepth = 0
        , enableHOF = True
        , pkgFetchOpts = defaultLocalOpts
        , modules = []
        , envPath = defaultEnvPath
        , hoPath = "ho.txt"
        }

defaultEnvPath = "data/env.db"
