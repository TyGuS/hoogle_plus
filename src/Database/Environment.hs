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
import Database.Util
import qualified Database.Download as DD
import qualified Database.Convert as DC
import Types.Environment (Environment, symbols, _symbols, _included_modules)
import Types.Program (BareDeclaration(..), Declaration(..), ConstructorSig(..))
import Types.Generate
import Synquid.Resolver (resolveDecls)

import Synquid.Util


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

-- getDeps will try its best to come up with the declarations needed to satisfy unmet type dependencies in ourEntries.
-- There are the entries in the current set of packages (allEntries), and the strategy to look at other packages.
getDeps :: PackageFetchOpts -> Map MdlName [Entry] -> [Entry] -> IO [Declaration]
getDeps Local{files=f} allEntries ourEntries = do
  let dependentEntries = DC.entryDependencies allEntries ourEntries (concat $ Map.elems allEntries)
  nubOrd <$> mapM (flip evalStateT 0 . DC.toSynquidDecl) dependentEntries
getDeps Hackage{packages=ps} allEntries ourEntries = do
  pkgsDeps <- mapM (\pkgName -> do
    pkgDeps <- nubOrd <$> DC.packageDependencies pkgName True
    entriesFromDeps <- concatMap (concat . Map.elems) <$> (mapM (flip DC.readDeclarations Nothing) pkgDeps)
    let dependentEntries = DC.entryDependencies allEntries ourEntries entriesFromDeps
    mapM (flip evalStateT 0 . DC.toSynquidDecl) dependentEntries
    ) ps
  return $ nubOrd $ concat pkgsDeps

newGenerateEnv :: GenerationOpts -> IO Environment
newGenerateEnv genOpts = do
    let useHO = enableHOF genOpts
    let pkgOpts = pkgFetchOpts genOpts
    let mdls = modules genOpts
    let mbModuleNames = if length mdls > 0 then Just mdls else Nothing
    pkgFiles <- getFiles pkgOpts
    allEntriesByMdl <- filesToEntries pkgFiles
    DD.cleanTmpFiles pkgOpts pkgFiles
    let entriesByMdl = filterEntries allEntriesByMdl mbModuleNames
    let ourEntries = nubOrd $ concat $ Map.elems entriesByMdl
    dependencyEntries <- getDeps pkgOpts allEntriesByMdl ourEntries
    putStrLn $ show dependencyEntries
    let moduleNames = Map.keys entriesByMdl
    let allCompleteEntries = concat (Map.elems entriesByMdl)
    let allEntries = nubOrd allCompleteEntries
    ourDecls <- mapM (\entry -> evalStateT (DC.toSynquidDecl entry) 0) allEntries
    let hooglePlusDecls = DC.reorderDecls $ nubOrd $ (ourDecls ++ dependencyEntries ++ defaultFuncs ++ defaultDts)
    case resolveDecls hooglePlusDecls moduleNames of
       Left errMessage -> error $ show errMessage
       Right env -> return env {
          _symbols = if useHO then env ^. symbols
                              else Map.map (Map.filter (not . isHigherOrder . toMonotype)) $ env ^. symbols,
         _included_modules = Set.fromList (moduleNames)
        }

   where
     filterEntries entries Nothing = entries
     filterEntries entries (Just mdls) = Map.filterWithKey (\m _-> m `elem` mdls) entries

-- filesToEntries reads each file into map of module -> declartions
-- Filters for modules we care about. If none, use them all.
filesToEntries :: [FilePath] -> IO (Map MdlName [Entry])
filesToEntries fps = do
    declsByModuleByFile <- mapM DC.readDeclarationsFromFile fps
    return $ Map.unionsWith (++) declsByModuleByFile


getFiles :: PackageFetchOpts -> IO [FilePath]
getFiles Hackage{packages=p} = mapM DD.getPkg p >>= (return . concat)
getFiles Local{files=f} = return f
