{-# LANGUAGE ScopedTypeVariables #-}

module Database.Environment
    ( writeEnv
    , generateEnv
    , toFunType
    , getFiles
    , filesToEntries
    ) where

import Control.Lens
import Control.Monad.State (evalStateT)
import qualified Data.ByteString as B
import Data.Either
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize (encode)
import qualified Data.Set as Set
import System.Exit (exitFailure)
import Text.Parsec.Pos (initialPos)
import Text.Printf

import qualified Data.List.Utils as LUtils
import qualified Database.Convert as DC
import qualified Database.Download as DD
import Database.Util
import qualified Debug.Trace as D
import Synquid.Error (Pos(Pos))
import Synquid.Logic (ftrue)
import Synquid.Pretty as Pretty
import Synquid.Resolver (resolveDecls)
import Synquid.Type (isHigherOrder, toMonotype)
import Synquid.Util
import Types.Common
import Types.Environment
import Types.Generate
import Types.Program (BareDeclaration(..), ConstructorSig(..), Declaration(..))
import qualified Types.Program as TP
import Types.Type -- (BaseType(..), TypeSkeleton(..), SchemaSkeleton(..))

writeEnv :: FilePath -> Environment -> IO ()
writeEnv path env = B.writeFile path (encode env)

-- getDeps will try its best to come up with the declarations needed to satisfy unmet type dependencies in ourEntries.
-- There are the entries in the current set of packages (allEntries), and the strategy to look at other packages.
getDeps :: PackageFetchOpts -> Map MdlName [Entry] -> [Entry] -> IO [Declaration]
getDeps Local {files = f} allEntries ourEntries = do
    let dependentEntries =
            DC.entryDependencies allEntries ourEntries (concat $ Map.elems allEntries)
    nubOrd <$> mapM (flip evalStateT 0 . DC.toSynquidDecl "") dependentEntries
getDeps Hackage {packages = ps} allEntries ourEntries = do
    pkgsDeps <-
        mapM
            (\pkgName -> do
                 pkgDeps <- nubOrd <$> DC.packageDependencies pkgName True
                 entriesFromDeps <-
                     concatMap (concat . Map.elems) <$>
                     (mapM (flip DC.readDeclarations Nothing) pkgDeps)
                 let dependentEntries = DC.entryDependencies allEntries ourEntries entriesFromDeps
                 mapM (flip evalStateT 0 . DC.toSynquidDecl "") dependentEntries)
            ps
    return $ nubOrd $ concat pkgsDeps

entriesToDecls :: Map Id [Entry] -> IO [TP.Declaration]
entriesToDecls m = do
    maps <- mapM entriesToDecls' (Map.toList m)
    return (concat maps)
    where
        entriesToDecls' :: (Id, [Entry]) -> IO [TP.Declaration]
        entriesToDecls' (mdl, entries) =
            mapM (\entry -> evalStateT (DC.toSynquidDecl mdl entry) 0) entries

instanceToDecls :: [Entry] -> IO [TP.Declaration]
instanceToDecls entries = do
    let instanceDecls = filter DC.isInstance entries
    let instanceRules = map DC.getInstanceRule instanceDecls
    let transitionIds = [0 .. length instanceRules]
    let instanceTuples = zip instanceRules transitionIds
    mapM instanceToDecls' instanceTuples
    where
        instanceToDecls' (entry, id) = 
            evalStateT (DC.instanceToFunction entry id) 0

tcToDecls :: [TP.Declaration] -> [Entry] -> IO ([TP.Declaration], [TP.Declaration])
tcToDecls ourDecls entries = do
    instanceFunctions <- instanceToDecls entries
    -- TODO: remove all higher kinded type instances
    let instanceFunctions' =
            filter
                (\x ->
                     not
                         (or [ (isInfixOf "Applicative" $ show x)
                             , (isInfixOf "Functor" $ show x)
                             , (isInfixOf "Monad" $ show x)
                              ]))
                instanceFunctions
    let declStrs = show (instanceFunctions' ++ ourDecls)
    let removeParentheses x = LUtils.replace ")" "" $ LUtils.replace "(" "" x
    let tcNames =
            nub $
            map removeParentheses $ filter (isInfixOf tyclassPrefix) (splitOn " " declStrs)
    let mkDecl x = Pos (initialPos "") $ TP.DataDecl "" x ["a"] [] []
    return $ (instanceFunctions', map mkDecl tcNames)

filterEnv :: GenerationOpts -> [Id] -> Environment -> IO Environment
filterEnv genOpts moduleNames env = do
    let useHO = enableHOF genOpts
    let pathToHo = hoPath genOpts
    let filterFun = if useHO then const True else (not . isHigherOrder . toMonotype)
    let filteredSymbols = Map.filter filterFun (env ^. symbols)
    let envFiltered = symbols .~ filteredSymbols $ env
    let envUpdated = included_modules .~ (Set.fromList (moduleNames)) $ envFiltered
    hofStr <- readFile pathToHo
    let hofNames = words hofStr
    -- get signatures
    let sigs = map (\f -> lookupWithError "env: symbols" f (envUpdated ^. symbols)) hofNames
    -- transform into fun types and add into the environments
    let sigs' = zipWith (\n t -> (n ++ hoPostfix, toFunType t)) hofNames sigs
    let envHofSym = symbols %~ Map.union (Map.fromList sigs') $ envUpdated
    let envHofCands = hoCandidates .~ map fst sigs' $ envHofSym
    return envHofCands

generateEnv :: GenerationOpts -> IO Environment
generateEnv genOpts = do
    let pkgOpts = pkgFetchOpts genOpts
    let mdls = modules genOpts
    let mbModuleNames = if length mdls > 0 then Just mdls else Nothing
    pkgFiles <- getFiles pkgOpts
    allEntriesByMdl <- filesToEntries pkgFiles True
    DD.cleanTmpFiles pkgOpts pkgFiles
    let entriesByMdl = filterEntries allEntriesByMdl mbModuleNames
    let allEntries = nubOrd $ concat $ Map.elems entriesByMdl
    dependencyEntries <- getDeps pkgOpts allEntriesByMdl allEntries
    let moduleNames = Map.keys entriesByMdl
    ourDecls <- entriesToDecls entriesByMdl
    (instanceFunctions, tcDecls) <- tcToDecls ourDecls allEntries
    let library = concat [ourDecls, dependencyEntries, instanceFunctions, tcDecls, defaultLibrary]
    let hooglePlusDecls = DC.reorderDecls $ nubOrd $ library
    result <-
        case resolveDecls hooglePlusDecls moduleNames of
            Left errMessage -> error $ show errMessage
            Right env -> filterEnv genOpts moduleNames env
    printStats result
    return result
  where
    filterEntries entries Nothing = entries
    filterEntries entries (Just mdls) = Map.filterWithKey (\m _ -> m `elem` mdls) entries

toFunType :: RSchema -> RSchema
toFunType (ForallT x t) = ForallT x (toFunType t)
toFunType (Monotype (FunctionT x tArg tRes)) =
    let tArg' = toMonotype $ toFunType $ Monotype tArg
        tRes' = toMonotype $ toFunType $ Monotype tRes
     in Monotype $ ScalarT (DatatypeT "Fun" [tArg', tRes'] []) ftrue
toFunType t = t

-- filesToEntries reads each file into map of module -> declartions
-- Filters for modules we care about. If none, use them all.
filesToEntries :: [FilePath] -> Bool -> IO (Map MdlName [Entry])
filesToEntries fps renameFunc = do
    declsByModuleByFile <- mapM (\fp -> DC.readDeclarationsFromFile fp renameFunc) fps
    return $ Map.unionsWith (++) declsByModuleByFile

getFiles :: PackageFetchOpts -> IO [FilePath]
getFiles Hackage {packages = p} = mapM DD.getPkg p >>= (return . concat)
getFiles Local {files = f} = return f

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
