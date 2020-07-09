{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Environment
    ( writeEnv
    , generateEnv
    , toFunType
    , getFiles
    , filesToEntries
    , writeFunction
    ) where

import Data.Char
import Data.Either
import Data.Serialize (encode)
import Data.List.Extra
import Control.Lens ((^.), over, _2)
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (evalStateT)
import System.Exit (exitFailure)
import Text.Parsec.Pos (initialPos)
import Text.Printf
import Debug.Trace

import Datalog.DatalogType
import Synquid.Error (Pos(Pos))
import Types.Type
import Types.Common
import Synquid.Type
import Synquid.Pretty
import Database.Utils
import qualified Database.Download as DD
import qualified Database.Convert as DC
import Types.Environment
import Types.Program (BareDeclaration(..), Declaration(..), ConstructorSig(..))
import Types.Generate
import Synquid.Resolver (resolveDecls)
import qualified Data.List.Utils as LUtils
import qualified Types.Program as TP
import Synquid.Utils
import HooglePlus.Utils
import qualified Debug.Trace as D

writeFunction :: PrintType a
              => String
              -> ([String] -> String)
              -> (TypeSkeleton -> a)
              -> Id
              -> TypeSkeleton
              -> String
writeFunction headerTempl printApp pkTyp f t | retVars `Set.isSubsetOf` argVars =
    if null args
       then printf "%s." (headClause "0")
       else printf "%s :- D >= 0, D <= {} %s %s."
               (headClause "D + 1")
               (if null depthVars then "" else printf ", D = %s" (intercalate " + " depthVars))
               (unwords (map ((',' :) . argClause) [0 .. (length args - 1)]))  :: String
    where
        monotype = typeSubstitute subst t
        ret = lastType monotype
        retVars = typeVarsOf ret
        headClause depth = printf headerTempl (writeType (pkTyp ret)) f (printApp progVars) depth :: String
        args = map snd (argsWithName monotype)
        argClause i = printf "sat(%s, P%d, D%d)" (writeType $ pkTyp (args !! i)) i i
        argVars = Set.unions $ map typeVarsOf args
        argNum = arity t - 1
        vars = Set.toList (typeVarsOf t)
        nextAvailable i = let v = "T" ++ show i
                           in if v `elem` vars then nextAvailable (i + 1) else (i + 1, v)
        (_, typVars) = foldr (\_ (i, lst) ->
            let (j, v) = nextAvailable i
             in (j, lst ++ [v])) (0, []) [0 .. (length vars - 1)]
        progVars = map (("P" ++) . show) [0 .. argNum]
        depthVars = map (("D" ++) . show) [0 .. argNum]
        subst = Map.fromList $ zipWith (\v1 v2 -> (v1, TypeVarT v2)) (filter ((`elem` ("D" : depthVars ++ progVars)) . map toUpper) vars) typVars
writeFunction _ _ _ _ _ = ""

writeEnv :: FilePath -> Environment -> IO ()
writeEnv path env =
    -- serialize environment into file
    B.writeFile path (encode env)

-- getDeps will try its best to come up with the declarations needed to satisfy unmet type dependencies in ourEntries.
-- There are the entries in the current set of packages (allEntries), and the strategy to look at other packages.
getDeps :: PackageFetchOpts -> Map MdlName [Entry] -> [Entry] -> IO [Declaration]
getDeps Local{files=f} allEntries ourEntries = do
  let dependentEntries = DC.entryDependencies allEntries ourEntries (concat $ Map.elems allEntries)
  nubOrd <$> mapM (flip evalStateT 0 . DC.toSynquidDecl) dependentEntries
getDeps Hackage{packages=ps} allEntries ourEntries = do
  pkgsDeps <- mapM (\pkgName -> do
    pkgDeps <- nubOrd <$> DC.packageDependencies pkgName True
    entriesFromDeps <- concatMap (concat . Map.elems) <$> mapM (`DC.readDeclarations` Nothing) pkgDeps
    let dependentEntries = DC.entryDependencies allEntries ourEntries entriesFromDeps
    mapM (flip evalStateT 0 . DC.toSynquidDecl) dependentEntries
    ) ps
  return $ nubOrd $ concat pkgsDeps

generateHigherOrder :: GenerationOpts -> Environment -> IO Environment
generateHigherOrder genOpts env = do
    let pathToHo = hoPath genOpts
    hofStr <- readFile pathToHo
    let hofNames = words hofStr
    -- get signatures
    let sigs = map (\f -> lookupWithError "env: symbols" f (env ^. symbols)) hofNames
    -- transform into fun types and add into the environments
    let sigs' = concat $ zipWith unfoldFuns hofNames sigs
    let env' = env { _symbols = Map.union (env ^. symbols) (Map.fromList sigs')
                   , _hoCandidates = map fst sigs' }
    return env'
    where
        newHoName name i = name ++ "_" ++ show i ++ hoPostfix

        mkFun acc (n, arg) = FunctionT n arg acc

        unfoldFuns name (ForallT x t) = map (over _2 (ForallT x)) (unfoldFuns name t)
        unfoldFuns name (Monotype t) = map (over _2 Monotype) $ snd $ unfoldFuns' name 0 [] t

        unfoldFuns' name i sofarArgs t@(FunctionT x tArg tRes) =
            let (i', sofar) = unfoldFuns' name i ((x,tArg):sofarArgs) tRes
                currHo = foldl mkFun (toFunType t) sofarArgs
             in (i' + 1, (newHoName name i', currHo):sofar)
        unfoldFuns' name i sofarArgs t = (i, [])

groupSymbols :: Environment -> Environment
groupSymbols env =
    let functions = env ^. symbols
        (gps, symGps, _) = Map.foldrWithKey addSignature (Map.empty, Map.empty, 0) functions
     in env {
            _groups = gps,
            _symbolGroups = symGps
        }
    where
        addSignature f sig (gps, symGps, i) =
            let alphaEquivs = Map.filter (eqType (toMonotype sig)) gps
             in case Map.size alphaEquivs of
                0 -> ( Map.insert ("g" ++ show i) (toMonotype sig) gps -- insert group and its signature
                     , Map.insert ("g" ++ show i) (Set.singleton f) symGps -- create a new group and add the function name to this group
                     , i + 1
                     )
                1 -> ( gps -- keep the group same
                     , Map.insertWith Set.union (head $ Map.keys gps) (Set.singleton f) symGps
                     , i
                     )
                _ -> error $ "more than one signature alpha equivalent to " ++ show sig ++ " : " ++ show (Map.elems alphaEquivs)

generateEnv :: GenerationOpts -> IO Environment
generateEnv genOpts = do
    let useHO = enableHOF genOpts
    let pkgOpts = pkgFetchOpts genOpts
    let mdls = modules genOpts
    let mbModuleNames = if not (null mdls) then Just mdls else Nothing
    pkgFiles <- getFiles pkgOpts
    allEntriesByMdl <- filesToEntries pkgFiles True
    DD.cleanTmpFiles pkgOpts pkgFiles
    let entriesByMdl = filterEntries allEntriesByMdl mbModuleNames
    let ourEntries = nubOrd $ concat $ Map.elems entriesByMdl
    dependencyEntries <- getDeps pkgOpts allEntriesByMdl ourEntries
    let moduleNames = Map.keys entriesByMdl
    let allCompleteEntries = concat (Map.elems entriesByMdl)
    let allEntries = nubOrd allCompleteEntries
    ourDecls <- mapM (\entry -> evalStateT (DC.toSynquidDecl entry) 0) allEntries

    let instanceDecls = filter DC.isInstance allEntries
    let instanceRules = map DC.getInstanceRule instanceDecls
    let transitionIds = [0 .. length instanceRules]
    let instanceTuples = zip instanceRules transitionIds
    instanceFunctions' <- mapM (\(entry, id) -> evalStateT (DC.instanceToFunction entry id) 0) instanceTuples

    -- TODO: remove all higher kinded type instances
    let instanceFunctions = filter (\x -> not(or [(isInfixOf "Applicative" $ show x),(isInfixOf "Functor" $ show x),(isInfixOf "Monad" $ show x)])) instanceFunctions'

    let declStrs = show (instanceFunctions ++ ourDecls)
    let removeParentheses = LUtils.replace ")" "" . LUtils.replace "(" ""
    let tcNames = nub $ map removeParentheses $ filter (isInfixOf tyclassPrefix) (splitOn " " declStrs)
    let tcDecls = map (\x -> Pos (initialPos "") $ TP.DataDecl x ["a"] []) tcNames

    let library = concat [ourDecls, dependencyEntries, instanceFunctions, tcDecls, defaultLibrary]
    let hooglePlusDecls = DC.reorderDecls $ nubOrd library

    result <- case resolveDecls hooglePlusDecls moduleNames of
       Left errMessage -> error $ show errMessage
       Right env -> do
            let env' = env { _symbols = if useHO then env ^. symbols
                                                else Map.filter (not . isHigherOrder . toMonotype) $ env ^. symbols,
                             _included_modules = Set.fromList moduleNames
                           }
            groupSymbols <$> generateHigherOrder genOpts env'
    printStats result
    -- print (result ^. symbols)
    return result
   where
     filterEntries entries Nothing = entries
     filterEntries entries (Just mdls) = Map.filterWithKey (\m _-> m `elem` mdls) entries

toFunType :: TypeSkeleton -> TypeSkeleton
toFunType (FunctionT x tArg tRes) = TyFunT tArg' tRes'
    where
        tArg' = toFunType tArg
        tRes' = toFunType tRes
toFunType t = t

-- filesToEntries reads each file into map of module -> declartions
-- Filters for modules we care about. If none, use them all.
filesToEntries :: [FilePath] -> Bool -> IO (Map MdlName [Entry])
filesToEntries fps renameFunc = do
    declsByModuleByFile <- mapM (`DC.readDeclarationsFromFile` renameFunc) fps
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
