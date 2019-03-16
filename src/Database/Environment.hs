{-# LANGUAGE ScopedTypeVariables #-}
module Database.Environment(writeEnv, generateEnv, newGenerateEnv) where

import Data.Either
import Data.Serialize (encode)
import Data.List.Extra
import Control.Lens ((^.))
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (evalStateT)
import System.Exit (exitFailure)
import Text.Parsec.Pos (initialPos)

import Synquid.Error (Pos(Pos))
import Synquid.Logic (ftrue)
import Types.Type (BaseType(..), TypeSkeleton(..), SchemaSkeleton(..))
import Synquid.Type (isHigherOrder, toMonotype)
import Synquid.Pretty as Pretty
import  Database.Util
import qualified Database.Download as DD
import qualified Database.Convert as DC
import Types.Environment (Environment, symbols, _symbols, _included_modules)
import Types.Program (BareDeclaration(..), Declaration(..), ConstructorSig(..))
import Types.Generate
import Synquid.Resolver (resolveDecls)


writeEnv :: FilePath -> Environment -> IO ()
writeEnv path env = B.writeFile path (encode env)

generateEnv :: [PkgName] -> [String] -> Int -> Bool -> IO Environment
generateEnv pkgs mdls depth useHO = do
  -- print pkgs
  pkgDecls <- mapM (\pkgName -> do
    DD.downloadFile pkgName Nothing >> DD.downloadCabal pkgName Nothing
    declMap <- DC.readDeclarations pkgName Nothing
    let fileDecls = concatMap (\mdl -> Map.findWithDefault [] mdl declMap) mdls
    parsedDecls <- mapM (\decl -> evalStateT (DC.toSynquidDecl decl) 0) fileDecls
    dependsPkg <- DC.packageDependencies pkgName True
    dependsDecls <- concatMap (concat . Map.elems) <$> (mapM (flip DC.readDeclarations Nothing) $ nub dependsPkg)
    additionalDts <- DC.declDependencies pkgName fileDecls dependsDecls >>= mapM (flip evalStateT 0 . DC.toSynquidDecl)
    return $ additionalDts ++ parsedDecls
    ) pkgs
  let decls = DC.reorderDecls $ nub $ defaultFuncs ++ defaultDts ++ concat pkgDecls
  case resolveDecls decls [] of
    Left resolutionError -> (pdoc $ pretty resolutionError) >> pdoc empty >> exitFailure
    Right env -> do
      return env {
          _symbols = if useHO then env ^. symbols
                              else Map.map (Map.filter (not . isHigherOrder . toMonotype)) $ env ^. symbols,
         _included_modules = Set.fromList mdls
        }
  where
    pdoc = putStrLn . show


newGenerateEnv :: GenerationOpts -> IO Environment
newGenerateEnv genOpts = do
    let pkgOpts = pkgFetchOpts genOpts
    let mdls = modules genOpts
    let mbModuleNames = if length mdls > 0 then Just mdls else Nothing
    pkgFiles <- getFiles pkgOpts
    entriesByMdl <- filesToEntries pkgFiles mbModuleNames
    let moduleNames = Map.keys entriesByMdl
    let allCompleteEntries = concat (Map.elems entriesByMdl)
    let allEntries = nubOrd allCompleteEntries
    hooglePlusDecls <- mapM (\entry -> evalStateT (DC.toSynquidDecl entry) 0) allEntries
    case resolveDecls hooglePlusDecls moduleNames of
       Left errMessage -> error $ show errMessage
       Right env -> return env

-- filesToEntries reads each file into map of module -> declartions
-- Filters for modules we care about. If none, use them all.
filesToEntries :: [FilePath] -> Maybe [MdlName] -> IO (Map MdlName [Entry])
filesToEntries fps mbMdls = do
    declsByModuleByFile <- mapM DC.readDeclarationsFromFile fps
    let declsByModule = Map.unionsWith (++) declsByModuleByFile
    let shouldKeepModule m _ = case mbMdls of
          Nothing -> True
          Just mdls -> m `elem` mdls
    return (Map.filterWithKey shouldKeepModule declsByModule)


getFiles :: PackageFetchOpts -> IO [FilePath]
getFiles Hackage{packages=p} = mapM DD.getPkg p >>= (return . concat)
getFiles Local{files=f} = return f
