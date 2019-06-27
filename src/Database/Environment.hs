{-# LANGUAGE ScopedTypeVariables #-}
module Database.Environment(writeEnv, generateEnv) where

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
import Types.Environment (Environment, symbols, _symbols, _included_modules, _typClassInstances, _condTypClasses, _datatypes)
import Types.Program (BareDeclaration(..), Declaration(..), ConstructorSig(..))
import Types.Generate
import Synquid.Resolver (resolveDecls)
import qualified Data.List.Utils as LUtils
import qualified Types.Program as TP
import Synquid.Util
import qualified Debug.Trace as D


writeEnv :: FilePath -> Environment -> IO ()
writeEnv path env = B.writeFile path (encode env)

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

generateEnv :: GenerationOpts -> IO Environment
generateEnv genOpts = do
    let useHO = enableHOF genOpts
    let pkgOpts = pkgFetchOpts genOpts
    let mdls = modules genOpts
    let mbModuleNames = if length mdls > 0 then Just mdls else Nothing
    pkgFiles <- getFiles pkgOpts
    allEntriesByMdl <- filesToEntries pkgFiles
    DD.cleanTmpFiles pkgOpts pkgFiles
    --print allEntriesByMdl
    let entriesByMdl = filterEntries allEntriesByMdl mbModuleNames
    --print "******************"
    --print mbModuleNames--entriesByMdl
    let ourEntries = nubOrd $ concat $ Map.elems entriesByMdl
    dependencyEntries <- getDeps pkgOpts allEntriesByMdl ourEntries
    putStrLn $ show dependencyEntries
    let moduleNames = Map.keys entriesByMdl
    let allCompleteEntries = concat (Map.elems entriesByMdl)
    let allEntries = nubOrd allCompleteEntries
    ourDecls <- mapM (\(entry) -> (evalStateT (DC.toSynquidDecl entry) 0)) allEntries

    let instanceDecls = filter (\entry -> DC.isInstance entry) allEntries
    let instanceRules = map DC.getInstanceRule instanceDecls
    let transitionIds = [0 .. length instanceRules]
    let instanceTuples = zip instanceRules transitionIds
    instanceFunctions <- mapM (\(entry, id) -> evalStateT (DC.instanceToFunction entry id) 0) instanceTuples

    -- TODO: remove all higher kinded types
    let instanceFunctions' = filter (\x -> not(or [(isInfixOf "Applicative" $ show x),(isInfixOf "Functor" $ show x),(isInfixOf "Monad" $ show x)])) instanceFunctions

    let declStrs = show (instanceFunctions' ++ ourDecls)
    let removeParentheses = (\x -> LUtils.replace ")" "" $ LUtils.replace "(" "" x)
    let tcNames = nub $ map removeParentheses $ filter (\x -> isInfixOf tyclassPrefix x) (splitOn " " declStrs)
    let tcDecls = map (\x -> Pos (initialPos "") $ TP.DataDecl x ["a"] [] []) tcNames


    let hooglePlusDecls = DC.reorderDecls $ nubOrd $ (ourDecls ++ dependencyEntries ++ defaultFuncs ++ defaultDts ++ instanceFunctions' ++ tcDecls)

    print "typeclass transitions: "
    print instanceFunctions'
    print "typeclass constructors: "
    print tcDecls
    print "decls"
    print hooglePlusDecls
    case resolveDecls hooglePlusDecls moduleNames of
       Left errMessage -> error $ show errMessage
       Right env -> D.trace (show $ (_datatypes env)) $ return env {
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
