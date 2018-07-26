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
import Distribution.PackDeps
import qualified Distribution.Version as DV

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


printDeclaration :: Environment -> Entry -> IO ()
printDeclaration env decl = case decl of
    EDecl (TypeSig _ names ty) -> do
        let typ = evalState (toSynquidSchema ty) 0
        putStrLn $ (nameStr (names !! 0)) ++ " :: " ++ (show typ)
        let styp = toSuccinctType $ evalState (toSynquidRType env ty) 0
        putStrLn $ "===> " ++ (show styp)
    EDecl decl -> print decl
    EPackage pkg -> putStrLn pkg
    EModule mdl -> putStrLn mdl

printDeclarations :: PkgName -> Maybe Version -> Environment -> IO ()
printDeclarations pkg version env = do
    decls <- readDeclations pkg version
    mapM_ (printDeclaration env) decls

typeSignatureOf :: Environment -> Entry -> Maybe (Id, RSchema)
typeSignatureOf env decl = case decl of
    EDecl (TypeSig _ names ty) -> do
        let typ = toSynquidRSchema env $ evalState (toSynquidSchema ty) 0
        Just (nameStr (names !! 0), typ)
    _ -> Nothing

emptyDtDef = DatatypeDef [] [] [] [] Nothing

-- packageEnv :: MonadIO m => PkgName -> StateT ExplorerState m Environment
-- packageEnv pkg = undefined
-- do
--     decls <- liftIO $ readDeclations pkg Nothing
--     let dts = Set.unions $ map getDeclTy decls
--     let env = foldr (uncurry addDatatype) emptyEnv (map withEmptyDt $ Set.toList dts)
--     let sigs = map fromJust . filter isJust . map (typeSignatureOf env) $ decls
--     let env' = foldr (uncurry addPolyVariable) env sigs
--     foldM (\accEnv (id, typ) -> addSuccinctSymbol id typ accEnv) env' sigs
--   where
--     withEmptyDt id = (id, emptyDtDef)
--     getDeclTy decl = case decl of
--         EDecl (TypeSig _ names ty) -> datatypeOf ty
--         _ -> Set.empty

packageTypes :: PkgName -> Set RType
packageTypes = undefined

packageDependency :: IO () -- IO (Map PkgName (Set PkgName))
packageDependency = do
    let pi = PackInfo (DV.Version [0,10,8,1] []) Nothing (0)
    let newest = Map.singleton "bytestring" pi
    let reverses = getReverses newest
    print reverses
