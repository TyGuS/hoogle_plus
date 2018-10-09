{-|
Module      : ImportUtils
Description : Provides utilities for parsing ASTs of import statements
-}

module ImportUtils where

import Language.Haskell.Exts (parseFile, ParseResult(..))
import Language.Haskell.Exts.Syntax (
  Module (..), 
  ImportDecl (..),
  ModuleName (..),
  Decl (..),
  Match (..),
  Name (..),
  Exp (..),
  Pat (..),
  QName (..),
  Rhs (..)
  )

-- TODO:
-- 1) Simplify import statements
-- 2) Simplify functions
-- 3) Add haddock docs


getImports (ParseOk (Module _ _ _ imports _)) = imports
getImports _                                  = []

getImportName (ImportDecl _ (ModuleName _ name) _ _ _ _ _ _) = name
getImportQualName (ImportDecl _ _ _ _ _ _ alias _ )          = alias

getImportsAndAliases fileAST = zip importNames importAliases
  where importASTs    = getImports fileAST
        importNames   = map getImportName     importASTs
        importAliases = map getImportQualName importASTs
        importTuples  = zip importNames importAliases
