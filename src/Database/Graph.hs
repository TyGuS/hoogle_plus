module Database.Graph where

import System.Environment
import Data.List.Extra
import Data.Maybe
import Data.Either
import Control.Monad.State
import Language.Haskell.Exts
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Debug.Trace
-- import Distribution.PackDeps
-- import qualified Distribution.Version as DV
-- import Distribution.Verbosity
-- import Distribution.PackageDescription
-- import Distribution.PackageDescription.Parsec
-- import Distribution.Package

import Synquid.Succinct
import Synquid.Pretty
import Synquid.Type
import Synquid.SolverMonad
import Synquid.Program hiding (TypeDecl)
import Synquid.Util
import Synquid.Logic
import Database.Convert
import Database.Generate
import Database.Util
import Database.Download


printDeclaration :: Entry -> IO ()
printDeclaration decl = case decl of
    EDecl (TypeSig _ names ty) -> do
        let typ = evalState (toSynquidSchema ty) 0
        putStrLn $ (nameStr (names !! 0)) ++ " :: " ++ (show typ)
        let styp = toSuccinctType $ evalState (toSynquidRType ty) 0
        putStrLn $ "===> " ++ (show styp)
    EDecl decl -> print decl
    EPackage pkg -> putStrLn pkg
    EModule mdl -> putStrLn mdl

printDeclarations :: PkgName -> Maybe Version -> IO ()
printDeclarations pkg version = do
    decls <- readDeclarations pkg version
    mapM_ printDeclaration decls

typeSignatureOf :: Entry -> Maybe (Id, RSchema)
typeSignatureOf decl = case decl of
    EDecl (TypeSig _ names ty) -> do
        let typ = toSynquidRSchema $ evalState (toSynquidSchema ty) 0
        Just (nameStr (names !! 0), typ)
    _ -> Nothing

emptyDtDef = DatatypeDef [] [] [] [] Nothing


packageTypes :: PkgName -> Set RType
packageTypes = undefined

