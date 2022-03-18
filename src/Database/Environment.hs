{-# LANGUAGE ScopedTypeVariables #-}
module Database.Environment
  ( 
    writeEnv
  , generateEnv
  , getFiles
  , filesToEntries

  , GroupResult(..)
  , emptyGroup
  , groupSignatures
  ) where

import           Control.Monad.State            ( StateT
                                                , evalState
                                                )
import           Data.Bifunctor                 ( second )
import           Data.Functor                   ( (<&>) )
import           Data.List.Extra                ( nubOrd )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Serialize                 ( encode )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           System.Exit                    ( exitFailure )
import           Text.Parsec.Pos                ( initialPos )
import           Text.Printf                    ( printf )

import qualified Data.List.Utils               as LUtils
import           Text.PrettyPrint.ANSI.Leijen   ( string )

import           Compiler.Resolver
import           Database.Convert
import           Types.Common
import           Types.Encoder
import           Types.Environment
import           Types.Fresh
import           Types.Generate
import           Types.Pretty
import           Types.Type
import           Utility.Utils

writeEnv :: Environment -> IO ()
writeEnv env = return ()


generateHigherOrder :: [Id] -> Map Id SchemaSkeleton -> Map Id SchemaSkeleton
generateHigherOrder hofNames components =
    -- get signatures
  let hoSigs = Map.filterWithKey (\k _ -> k `elem` hofNames) components
  -- transform into fun types and add into the environments
      sigs'  = concat $ Map.mapWithKey unfoldFun hoSigs
  in  Map.fromList sigs'
 where
  newHoName :: Id -> Int -> Id
  newHoName name i =
    Text.pack $ Text.unpack name ++ "_" ++ show i ++ Text.unpack hoPostfix

  unfoldFun :: Id -> SchemaSkeleton -> [(Id, SchemaSkeleton)]
  unfoldFun name (ForallT x sch) =
    map (second $ ForallT x) (unfoldFun name sch)
  unfoldFun name (Monotype t) =
    map (second Monotype) (snd $ unfoldFun' name 0 [] t)

  unfoldFun'
    :: Id
    -> Int
    -> [(Id, TypeSkeleton)]
    -> TypeSkeleton
    -> (Int, [(Id, TypeSkeleton)])
  unfoldFun' name i sofarArgs t@(FunctionT x tArg tRes) =
    let (i', sofar) = unfoldFun' name i ((x, tArg) : sofarArgs) tRes
        mkFun res (xArg, arg) = FunctionT xArg arg res
        currHo = foldl mkFun (toAbstractFun t) sofarArgs
    in  (i' + 1, (newHoName name i', currHo) : sofar)
  unfoldFun' name i sofarArgs t = (i, [])

generateEnv :: GenerationOpts -> IO ()
generateEnv genOpts = do
  let useHO         = enableHOF genOpts
  let pkgOpts       = pkgFetchOpts genOpts
  let mdls          = modules genOpts
  let mbModuleNames = if not (null mdls) then Just mdls else Nothing
  pkgFiles        <- getFiles pkgOpts
  allEntriesByMdl <- filesToEntries pkgFiles True
  let entriesByMdl    = filterEntries allEntriesByMdl mbModuleNames
  let entries         = nubOrd $ concat $ Map.elems entriesByMdl
  let declarations    = map ((`evalState` 0) . toDeclaration) entries
  let hooglePlusDecls = reorderDecls $ nubOrd declarations

  result <- case resolveDecls hooglePlusDecls of
    Left  errMessage -> error $ show errMessage
    Right env        -> do
      let symbols   = getSymbols env
      let foSymbols = Map.filter (not . isHigherOrder . toMonotype) symbols
      let env' = env { getSymbols = if useHO then symbols else foSymbols }
      hofNames <- if useHO
        then readFile (hoPath genOpts) <&> lines
        else return []
      let hoSigs = generateHigherOrder (map Text.pack hofNames) symbols
      return env'

  printStats result
  writeEnv result
 where
  filterEntries entries Nothing = entries
  filterEntries entries (Just mdls) =
    Map.filterWithKey (\m _ -> m `elem` mdls) entries

  isPolymorphic (ForallT _ _) = True
  isPolymorphic _             = False


--------------------------------------------------------------------------------
------------------------------ Component Grouping ------------------------------
--------------------------------------------------------------------------------

data GroupResult = GroupResult
  { groupMap    :: Map GroupId (Set Id) -- mapping from group id to Skel and list of function names with the same skel
  , typeToGroup :: Map EncodedFunction GroupId
  , nameToGroup :: Map Id GroupId
  }
  deriving Eq

emptyGroup :: GroupResult
emptyGroup = GroupResult { groupMap    = Map.empty
                         , typeToGroup = Map.empty
                         , nameToGroup = Map.empty
                         }

groupPrefix :: Id
groupPrefix = "gm"

data FunctionGroup = FunctionGroup GroupId EncodedFunction (Set Id)
  deriving Eq

mkFunctionGroup :: GroupId -> (EncodedFunction, Set Id) -> FunctionGroup
mkFunctionGroup gid (f, fs) = FunctionGroup gid f fs

getFunctionSet :: FunctionGroup -> Set Id
getFunctionSet (FunctionGroup _ _ ids) = ids

toGroupMap :: FunctionGroup -> (GroupId, Set Id)
toGroupMap (FunctionGroup gid _ ids) = (gid, ids)

toTypeMap :: FunctionGroup -> (EncodedFunction, GroupId)
toTypeMap (FunctionGroup gid f _) = (f, gid)

toNameMap :: FunctionGroup -> Map Id GroupId
toNameMap (FunctionGroup gid _ ids) = Map.fromSet (const gid) ids

groupSignatures
  :: Fresh s m => Map Id EncodedFunction -> StateT s m GroupResult
groupSignatures sigs = do
  -- group signatures by their types
  let sigGroups = Map.toList $ Map.map Set.fromList $ groupByMap sigs
  writeLog 3 "groupSignatures" $ pretty sigGroups

  -- give each group a unique id
  groupNames <- mapM (\_ -> freshId [] groupPrefix) sigGroups
  let namedSigGroups = zipWith mkFunctionGroup groupNames sigGroups
  let allIds         = map (Set.size . getFunctionSet) namedSigGroups
  let dupes          = filter (> 1) allIds
  writeLog 3 "groupSignatures" $ string $ printf
    "%d class; %d equiv; %d total"
    (length sigGroups)
    (sum dupes)
    (Map.size sigs)

  let groupMap = Map.fromList $ map toGroupMap namedSigGroups
  let t2g      = Map.fromList $ map toTypeMap namedSigGroups
  let n2g      = Map.unions $ map toNameMap namedSigGroups
  return $ GroupResult groupMap t2g n2g

-- filesToEntries reads each file into map of module -> declartions
-- Filters for modules we care about. If none, use them all.
filesToEntries :: [FilePath] -> Bool -> IO (Map MdlName [Entry])
filesToEntries fps renameFunc = do
  declsByModuleByFile <- mapM (`readDeclarationsFromFile` renameFunc) fps
  return $ Map.unionsWith (++) declsByModuleByFile


getFiles :: PackageFetchOpts -> IO [FilePath]
getFiles Local { files = f } = return f

printStats :: Environment -> IO ()
printStats env = do
  let symbols      = getSymbols env
  let symbolsCount = Map.size symbols
  let polySymbols  = Map.filter isPolymorphic symbols
  let polyCount    = Map.size polySymbols
  let datatypes =
        Set.unions $ map (allDatatypes . toMonotype) $ Map.elems symbols
  let typeCount = Set.size datatypes

  printf "types: %d; symbols: %d; polymorphic symbols: %d\n"
         typeCount
         symbolsCount
         polyCount
  printf "included types: %s\n" $ show (Set.toList datatypes)
