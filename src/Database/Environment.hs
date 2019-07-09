{-# LANGUAGE ScopedTypeVariables #-}
module Database.Environment(writeEnv, writeGraph, generateEnv, toFunType) where

import Data.Either
import Data.Serialize (encode)
import Data.List.Extra
import Control.Lens ((^.))
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import System.Exit (exitFailure)
import Text.Parsec.Pos (initialPos)
import Text.Printf
import qualified Debug.Trace as D
import Control.Concurrent.Chan

import Synquid.Error (Pos(Pos))
import Synquid.Logic (ftrue)
import Types.Type -- (BaseType(..), TypeSkeleton(..), SchemaSkeleton(..))
import Synquid.Type (isHigherOrder, toMonotype)
import Synquid.Pretty as Pretty
import Database.Util
import qualified Database.Download as DD
import qualified Database.Convert as DC
import Types.Environment
import Types.Generate
import PetriNet.PNSolver
import Types.Solver
import qualified HooglePlus.Abstraction as Abstraction
import Synquid.Resolver (resolveDecls)
import qualified Data.List.Utils as LUtils
import Types.Program
import Synquid.Util
import Synquid.Program

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

writeGraph :: FilePath -> Environment -> IO ()
writeGraph solverPath env = do
    messageChan <- newChan
    let is = emptySolverState
                { _abstractionCover = Abstraction.firstLvAbs env (Map.elems (allSymbols env))
                , _messageChan = messageChan
                }
    solverState <- execStateT (initNet env) is
    B.writeFile solverPath (encode $ toWritable solverState)
    where
        toWritable st = WritableSolverState
            {
                _nc = st ^. nameCounter,
                _sigs = st ^. currentSigs,
                _as = st ^. activeSigs,
                _fm = st ^. functionMap,
                _gm = st ^. groupMap,
                _gr = st ^. groupRepresentative,
                _t2g = st ^. typeToGroup,
                _n2g = st ^. nameToGroup,
                _t2tr = st ^. type2transition,
                _nm = st ^. nameMapping,
                _im = st ^. instanceMapping,
                _ic = st ^. instanceCounts
            }

generateEnv :: GenerationOpts -> IO Environment
generateEnv genOpts = do
    let useHO = enableHOF genOpts
    let pkgOpts = pkgFetchOpts genOpts
    let mdls = modules genOpts
    let pathToHo = hoPath genOpts
    let mbModuleNames = if length mdls > 0 then Just mdls else Nothing
    pkgFiles <- getFiles pkgOpts
    allEntriesByMdl <- filesToEntries pkgFiles
    DD.cleanTmpFiles pkgOpts pkgFiles
    let entriesByMdl = filterEntries allEntriesByMdl mbModuleNames
    let ourEntries = nubOrd $ concat $ Map.elems entriesByMdl
    dependencyEntries <- getDeps pkgOpts allEntriesByMdl ourEntries
    let moduleNames = Map.keys entriesByMdl
    let allCompleteEntries = concat (Map.elems entriesByMdl)
    let allEntries = nubOrd allCompleteEntries
    ourDecls <- mapM (\(entry) -> (evalStateT (DC.toSynquidDecl entry) 0)) allEntries

    let instanceDecls = filter (\entry -> DC.isInstance entry) allEntries
    let instanceRules = map DC.getInstanceRule instanceDecls
    let transitionIds = [0 .. length instanceRules]
    let instanceTuples = zip instanceRules transitionIds
    instanceFunctions <- mapM (\(entry, id) -> evalStateT (DC.instanceToFunction entry id) 0) instanceTuples

    -- TODO: remove all higher kinded type instances
    let instanceFunctions' = filter (\x -> not(or [(isInfixOf "Applicative" $ show x),(isInfixOf "Functor" $ show x),(isInfixOf "Monad" $ show x)])) instanceFunctions

    let declStrs = show (instanceFunctions' ++ ourDecls)
    let removeParentheses = (\x -> LUtils.replace ")" "" $ LUtils.replace "(" "" x)
    let tcNames = nub $ map removeParentheses $ filter (\x -> isInfixOf tyclassPrefix x) (splitOn " " declStrs)
    let tcDecls = map (\x -> Pos (initialPos "") $ DataDecl x ["a"] [] []) tcNames

    let library = concat [ourDecls, dependencyEntries, instanceFunctions', tcDecls, defaultLibrary]
    let hooglePlusDecls = DC.reorderDecls $ nubOrd $ library

    result <- case resolveDecls hooglePlusDecls moduleNames of
       Left errMessage -> error $ show errMessage
       Right env -> do
            let env' = env { _symbols = if useHO then env ^. symbols
                                                else Map.filter (not . isHigherOrder . toMonotype) $ env ^. symbols,
                           _included_modules = Set.fromList (moduleNames)
                          }
            hofStr <- readFile pathToHo
            let hofNames = words hofStr
            -- get signatures
            let sigs = map (\f -> lookupWithError "env: symbols" f (env' ^. symbols)) hofNames
            -- transform into fun types and add into the environments
            let sigs' = zipWith (\n t -> (n ++ hoPostfix, toFunType t)) hofNames sigs
            let env'' = env' { _symbols = Map.union (env' ^. symbols) (Map.fromList sigs')
                             , _hoCandidates = map fst sigs' }
            return env''
    printStats result
    return result
   where
     filterEntries entries Nothing = entries
     filterEntries entries (Just mdls) = Map.filterWithKey (\m _-> m `elem` mdls) entries

toFunType :: RSchema -> RSchema
toFunType (ForallT x t) = ForallT x (toFunType t)
toFunType (Monotype (FunctionT x tArg tRes)) = let
  tArg' = toMonotype $ toFunType $ Monotype tArg
  tRes' = toMonotype $ toFunType $ Monotype tRes
  in Monotype $ ScalarT (DatatypeT "Fun" [tArg', tRes'] []) ftrue
toFunType t = t

-- filesToEntries reads each file into map of module -> declartions
-- Filters for modules we care about. If none, use them all.
filesToEntries :: [FilePath] -> IO (Map MdlName [Entry])
filesToEntries fps = do
    declsByModuleByFile <- mapM DC.readDeclarationsFromFile fps
    return $ Map.unionsWith (++) declsByModuleByFile


getFiles :: PackageFetchOpts -> IO [FilePath]
getFiles Hackage{packages=p} = mapM DD.getPkg p >>= (return . concat)
getFiles Local{files=f} = return f

printStats :: Environment -> IO ()
printStats env = do
  let typeMap = env ^. datatypes
  let modules = _included_modules env
  let typeclassInstances = _typClassInstances env
  let symbols = _symbols env
  let symbolsCount = Map.size symbols
  let typeCount = Map.size typeMap
  printf "types: %d; symbols: %d\n" typeCount symbolsCount
  printf "included types: %s\n" $ show (Map.keys typeMap)
  printf "included modules: %s\n" $ show (Set.elems modules)