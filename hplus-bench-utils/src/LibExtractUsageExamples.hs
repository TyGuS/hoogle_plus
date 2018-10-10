{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module LibExtractUsageExamples
    ( extractUsageExamples,  extractImports
    ) where
import Debug.Trace
import Language.Haskell.Exts (parseFile, ParseResult(..), parseFileContents, ParseMode(..), defaultParseMode, parseFileWithMode)
import Language.Haskell.Exts.Extension (Extension (..), KnownExtension (..),Language (..))
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
  Rhs (..),
  Literal (..)
  )
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Fixity (preludeFixities, baseFixities)
import System.Environment (getArgs)
import Language.Haskell.Exts.ExactPrint (exactPrint)
import Data.Foldable (foldl, foldMap)
import Data.Generics.Uniplate.Data
import Control.Monad.Writer
import System.IO
import Language.Haskell.Ghcid
import Hoogle
import Control.Monad.State
import ImportUtils
import AppUtils
import Data.List
import Data.List.Split
import qualified Data.Text as T
import Control.Monad
import CsvUtils
import qualified Data.Vector as Vector
import Data.Char (toLower)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Language.Haskell.Exts.CPP
import Data.Git.Storage (findRepoMaybe, openRepo, gitRepoPath)


getCurrentGitRepoPath :: IO Filepath
getCurrentGitRepoPath = do
    mbPath <- findRepoMaybe
    mbRepo <- mbPath <$> openRepo
    mbRootPath <- mbRepo <$> gitRepoPath
    case mbRootPath of
        Just rootPath -> rootPath
        Nothing -> error "you done goofed. get it in git"

-- TODO
-- 1. Save to CSV of format (example, type, timestamp, origin filename)
-- 2. Parametrize CSV to format (original_filename_benchmarks.csv)
-- 3. Add function signatures and reformat by moving functions around

getFunctionApplications :: Foldable t =>  String -> ParseResult (Module l) ->[(String, String)] -> [String] -> t String -> IO [String]
getFunctionApplications intermediateOutput fileAST qualToImportNameMap modules supportedFunctions = do

  let allApps          = getAllApplications fileAST
  let variablesPerApp  = map getIdentifiers allApps
  let variablesPerApp' = map nub variablesPerApp
   
  let constructAliasMap   = getAliasMap modules qualToImportNameMap  supportedFunctions intermediateOutput
  aliasesTuple            <- mapM constructAliasMap variablesPerApp'
  let (numArgsPerExample, aliasesPerExample) = unzip aliasesTuple

  let exampleData     = zip3 allApps numArgsPerExample aliasesPerExample
  let parametrizeArgs = filter usesLibrary exampleData

  let allNames = concatMap (universeBi :: (Exp () -> [QName ()])) allApps
  let allUnQualifiedNames  = filter isUnQualifiedName allNames
  let allUnQualifiedNames' = map extractNameStr allUnQualifiedNames
  let allUnQualifiedNames''= filter (\x -> elem x supportedFunctions) allUnQualifiedNames'

  let getGHCImoduleName' = getGHCImoduleName intermediateOutput
  unQualifiedMap <- mapM getGHCImoduleName'  (nub allUnQualifiedNames'')

  let removeUnsupportedFunctions = parametrize unQualifiedMap qualToImportNameMap
  let unamedExamples = map removeUnsupportedFunctions parametrizeArgs
  let libraryUsageExamples = map prettyPrint unamedExamples
  return libraryUsageExamples
  where turnIntoFunction (ast ,idx) = FunBind () [Match () (Ident () ("f" ++ show idx)) [] (UnGuardedRhs () ast) Nothing]
        isUnQualifiedName (UnQual _ _) = True
        isUnQualifiedName _            = False
        extractNameStr (UnQual () (Ident () str)) = str
        extractNameStr _ = ""
        usesLibrary (ast, numArgs, aliasMap) = (numArgs /= 0) && (numArgs /= length aliasMap)


computeFilePaths filePath = 
  let candidates      = tail $ splitOn "/" $ head $ splitOn "." filePath
      numCandidates   = length candidates
      baseName        = candidates !! 0
      filePathIsValid = numCandidates == 1 
      outputPath      = "./outputs/" ++ baseName ++ ".csv"
      tempOutputPath  = "./intermediates/" ++ baseName ++ ".hs"
      resultTuple     = (tempOutputPath, outputPath)
      result = if filePathIsValid then resultTuple else error errMessage
      in result
  where errMessage = "filePath was not valid"


loadHashMaps name= do
  let dataDir = "/home/djusto/research/data/"
  --let inputsDir  = dataDir ++ "inputs/hackage500Maps/"
  let workingDir = dataDir ++ "working/hackage500Maps/"

  let qualMapPath = workingDir ++ name ++ "_qual.json"
  let explMapPath = workingDir ++ name ++ "_expl.json"

  qualMap <- B.readFile qualMapPath
  explMap <- B.readFile explMapPath
  let qualMap' = (decode qualMap ):: Maybe (HM.HashMap String String)
  let explMap' = (decode explMap ):: Maybe (HM.HashMap String String)
  case (qualMap', explMap') of
    (Just x, Just y) -> return (x, y)
    _                -> error "jsons were faulty"


transformF :: HM.HashMap String String -> HM.HashMap String String -> (QName ()) -> (QName ())
transformF qualMap explicitMap identifier = 
  let defaultName = UnQual () (Ident () "arg")
      in case identifier of
              Qual () (ModuleName () mName) (Ident () name) -> case HM.lookup mName qualMap of
                                                               Just mName' -> Qual () (ModuleName () mName') (Ident () name)
                                                               _           -> defaultName
              UnQual () (Ident () name) -> case HM.lookup name explicitMap of
                                                Just path -> let mName' = case (splitLast '.' path) of
                                                                                Right (a,b) -> a
                                                                                _           -> error "unknown"
                                                                 in Qual () (ModuleName () mName') (Ident () name)

                                                _         -> defaultName
              _ -> defaultName

transformPat :: (Pat ()) -> (Pat ())
transformPat identifier = case identifier of
  PVar () (Ident () name) -> PVar () (Ident () "arg") 
  _ -> PVar () (Ident () "arg")



transformLit :: (Literal ()) -> (Literal ())
transformLit identifier = case identifier of
  _ -> Language.Haskell.Exts.Syntax.String () "arg" "arg"

{--
transformF4 :: (Name ()) -> Name ()
transformF4 name = case name of
  Ident () name -> let isPresent = case lookup name <> of
                                     Just _ -> True
                                     _      -> False
                       name' = if isPresent then "arg" else name
                       in Ident () name'
  _ -> Ident () "arg"
--}

getAllApplications' declASTs = allFunctionApplications
  where functionDeclASTs = filter isFunctionDecl declASTs
        getExpList x     = (universeBi x)::([Exp ()])
        expsPerFunction  = map getExpList functionDeclASTs

        getAppList x     = [app | Just app <- (map maybeFuncApp x)]
        appsPerFunction  = map getAppList expsPerFunction
        allFunctionApplications = concat appsPerFunction

extractImports = do

  filePath <- getFilePath
  let fileNameArr = (splitOn "/" $ head $ splitOn ".hs" filePath)
  let fileName = fileNameArr !! (length fileNameArr -1) 
  --print fileName
  let outDir = "/home/djusto/research/data/working/hackage500Imports/" 
  let outPath = outDir ++ fileName ++ ".txt" 



  fileAST <- parseFileWithCommentsAndCPP defaultCpphsOptions (defaultParseMode { fixities = Just (preludeFixities ++ baseFixities)}) filePath

  case fileAST of
    ParseOk ((Module _ _ _ importDecl _),_) -> let importDecl' = map (fmap $ const ()) importDecl
                                                   toSave      = unlines $ map prettyPrint importDecl'
                                                   in writeFile outPath toSave
    _ -> print $ fileAST

extractUsageExamples = do
  filePath <- getFilePath

  let fileNameArr = (splitOn "/" $ head $ splitOn ".hs" filePath)
  let fileName = fileNameArr !! (length fileNameArr -1) 

  let outDir = "/home/djusto/research/data/working/hackage500CSVs/" 
  let outPath = outDir ++ fileName ++ ".csv"

  (qualMap, explMap) <- loadHashMaps fileName

  ParseOk (fileAST,_) <- parseFileWithCommentsAndCPP defaultCpphsOptions (defaultParseMode { fixities = Just (preludeFixities ++ baseFixities)}) filePath
  --fileAST <- parseFileWithMode (defaultParseMode {baseLanguage = Haskell2010, extensions = [EnableExtension CPP, EnableExtension ScopedTypeVariables, EnableExtension LambdaCase]}) filePath

  let fileAST' = fmap (const ()) fileAST
  let declarations = getDecls' fileAST
  let declarations' = map (fmap $ const ()) declarations

  let transformF' = transformF qualMap explMap
  let declarations'' = transformBi transformF' declarations'
  let declarations''' = transformBi transformPat declarations''
  let declarations'''' = transformBi transformLit declarations'''

  let allApps          = getAllApplications' declarations''''


  let csvItems = map (\x -> Item{expa=(T.pack x), typeSig=(T.pack "unavailable")}) $ map prettyPrint allApps
  encodeItemsToFile outPath (Vector.fromList csvItems)

  --print filePath

  return () 
 




{--
-- | 'extractUsageExamples' is the main driver of the benchmark extraction
-- utility. If the executable was called correctly, it will analyze a Haskell
-- source file for all instances (and sub-instances) of function applications
-- for the supported libraries. It will then only parametrize those identifiers
-- which are not defined in the supported libraries. Lastly, it will write
-- all examples of function applications into a new file.
--extractUsageExamples = do

  -- Get path to a haskell source code file
  -- If valid, compute filepaths to temporary and final output files
  --filePath <- getFilePath
  --let (tempOutputPath, outputPath) = computeFilePaths filePath

  -- Parametrize stream to temporary file
  --let writeToTempFile = writeBenchmarks tempOutputPath
   
  -- We get a file-level AST
  -- We get a tuple of the supported modules and unqualified function names
  --fileAST                       <- parseFile filePath
  --(modules, supportedFunctions) <- getSupportedModules

  -- Extract import-level information
  -- TODO: clean up this tuple and make names more semantic
  --let (printableImports, qualToImportNameMap, importNames) = getImportData fileAST modules
  --writeToTempFile printableImports
  
  -- get examples of function applications
  --let getFunctionApplications' = getFunctionApplications tempOutputPath
  --libraryUsageExamples <- getFunctionApplications' fileAST qualToImportNameMap modules supportedFunctions
  --let pureImports = map (\x -> "import " ++ x) importNames
  --riteToTempFile pureImports

  -- save these examples
  -- writeBenchmarks' libraryUsageExamples

  -- CSV save

  --let names = [ "f" ++ show num | num <- [0..length libraryUsageExamples -1]]

  --print "B4"
  --let getGHCIType' = getGHCIType tempOutputPath
  --typeSigs <- mapM getGHCIType' libraryUsageExamples --names

  --let libraryUsageExamples' = zip libraryUsageExamples typeSigs
  --let csvItems = map (\(x, y) -> Item{expa=(T.pack x), typeSig=(T.pack y)}) libraryUsageExamples'
  --               where libraryUsageExamples' = zip typeSigs libraryUsageExamples
  --encodeItemsToFile outputPath (Vector.fromList csvItems)
  --return ()
--}


isSupportedImport supportedModules importAST =
  case importAST of
    importAST@(ImportDecl _ (ModuleName _ name) _ _ _ _ _ __) -> isSupported
      where isSupported = name `elem` supportedModules
    _ -> False

parametrize qualMap kk (appAST, numArgs, renamingMap) = parametrizedAST

  where argsList         = map createArg [0..(numArgs-1)]
        --parametrizedBody = transformBi (renameIdentifier renamingMap) appAST
        --parametrizedBody'= transformBi (qualifyIdentifier qualMap kk) parametrizedBody
        parametrizedBody' = transformBi (qualifyIdentifier' renamingMap) appAST
        parametrizedAST  = Lambda () argsList parametrizedBody'
        createArg num    = PVar () (Ident () ("x" ++ show num))

renameIdentifier :: [(String, String)] -> Name () -> Name ()
renameIdentifier renamingMap nameAST =
  case nameAST of
    nameAST@(Ident () str) -> Ident () (f name)
      where name = lookup str renamingMap
            f (Just a)  = a
            f Nothing   = str
    _ -> nameAST


qualifyIdentifier' :: [(QName (), QName ())] -> QName () -> QName ()
qualifyIdentifier' renamingMap ast = 
  case lookup ast renamingMap of
    Just ast' -> ast'
    Nothing   -> case ast of
                   ast@(UnQual () (Symbol _ _)) -> ast
                   _ -> error "qualMap was incomplete"

qualifyIdentifier :: [(String, String)]-> [(String, String)] -> QName () -> QName ()
qualifyIdentifier qualMap importAliases nameAST =
  case nameAST of
    nameAST@(Qual _ (ModuleName _ mName) (Ident _ name)) -> qual'
      where qual'  = Qual () (ModuleName () mName') (Ident () name)
            mName' = case lookup mName importAliases of
                          Just x -> x
                          _      -> mName
    nameAST@(UnQual _ (Ident _ name)) -> qual'
      where qual'  = case lookup name qualMap of
                       Just x -> Qual () (ModuleName () x) (Ident () name)
                       _ -> nameAST
    _ -> nameAST



takeRNaive :: Int -> [a] -> [a]
takeRNaive n = reverse . take n . reverse 




getAliasMap :: (Foldable t, Num a, Show a) => [String] -> [(String, String)] -> t String -> String ->[(QName (), String)] -> IO (a, [(QName (), QName ())])
getAliasMap modules qualMap namesToPreserve intermediateOutput names = do
  let uniqueNames = nub names
  let uniqueNames' = map f uniqueNames
  let generateMap = step namesToPreserve
  result <- foldM generateMap (0, []) uniqueNames'
  return result
  where f (a@(Qual _ (ModuleName _ mName) _), b) = (mName, prettyPrint a, b, a)
        f (a@(UnQual _ _), b)     = (""   , prettyPrint a, b, a) 
        step set acc name = do
                     let (idx, aliasMap) = acc
                     let (mName, prettyP, name', var) = name

                     let isInSet = name' `elem` set
                     let validQuOp1 = mName `elem` modules
                     let validQuOp2 = case lookup mName qualMap of
                                        Just name -> name `elem` modules
                                        Nothing   -> False
                     let validQu = validQuOp1 || validQuOp2
                     

                     let f' = getGHCImoduleName intermediateOutput 
                     secondaryCond <- do{ if isInSet then (\(_, x) -> x `elem` modules) <$> (f' prettyP)  else return False}
                     let finalCond = if validQu then True else secondaryCond 
                     let idx'    = if finalCond then idx else idx + 1
                     let arg     = "x" ++ show idx



                     (_, mName'') <- if finalCond then f' prettyP else return ("","")
                     let tupl1   = (var, (Qual () (ModuleName () mName'') (Ident () name')))

                     let tupl2   = (var, (UnQual () (Ident () arg)))
                     let aliasTupl = if finalCond then tupl1 else tupl2
                     let aliasMap' = aliasTupl:aliasMap
                     let result' = (idx', aliasMap')
                     return result'

                {-
                 let (idx, aliasMap) = acc
                                  isInSet = name `elem` set
                                  idx'    = if isInSet then idx else idx + 1
                                  arg     = "x" ++ show idx
                                  tupl1   = (name, name)
                                  tupl2   = (name, arg)
                                  aliasTupl = if isInSet then tupl1 else tupl2
                                  aliasMap' = aliasTupl:aliasMap
                              in (idx', aliasMap') -}


-- | 'writeBenchmarks' takes as input a list of strings.
-- The prints all strings in a newline to the a file.
-- TODO: parametrize filename.
-- TODO: give better function name.
writeBenchmarks fname apps = writeFile fname (unlines apps)


-- =========================================== Fixed?

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Here
-- | 'getFilename' is an argument-parsing helper. The correct
-- usage of the benchmark-extraction tool is to call the executable
-- followed by a string of the path to the file which to analyze.
-- If the executable was called correctly, this function returns
-- the path of the file analyze. Otherwise, an error is raised.
getFilePath :: IO String
getFilePath = do
  args <- getArgs
  case args of
    [filename] -> return filename
    _          -> error errorMessage
  where errorMessage = "Too many arguments"


-- | 'getSupportedModules' returns information about the supported libraries.
-- Namely, it returns a tuple where the first element is a list of supported
-- library modules and the second element is a list of supported (unqualified)
-- function names belonging to those modules.
-- TODO: think of a better name. One that encapsulates that itrs return value
--       is a tuple.
-- TODO: rename the argument of 'readFile'
getSupportedModules :: IO ([String], [String])
getSupportedModules = do
  contents <- readFile "src/bytestring.txt"
  let declarations  = lines contents
      isFunction    = isInfixOf "::"
      functions     = filter isFunction declarations
      extractName   = head . splitOn "::"
      functionNames = map extractName functions
      modulesAndFunctions = map (splitLast '.') functionNames
      modulesToFunctions  = [ (m, f) | Right (m,f) <- modulesAndFunctions]
      modules             = nub [ m | (m, _) <- modulesToFunctions]
      fnames              = map strip (nub [ n | (_, n) <- modulesToFunctions])
      in return (modules, fnames)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Import Utils

-- | 'getImportData' returns information of import statements in the
-- file-level AST. The first element of the returned 2-tuple are ASTs
-- of the import statements. The second element is a map from module aliases
-- to their expanded module path names.
getImportData :: Foldable t => ParseResult (Module l) -> t String -> ([String], [(String, String)], [String])
getImportData fileAST modules =
      -- Extracts the ASTs of import statements in the file-level
      -- AST. Then, it removes its annotations. Afterwards, it
      -- removes imports of non-supported modules.
  let allImports  = getImports fileAST
      allImports' = map Control.Monad.void allImports
      isSupported = isSupportedImport modules
      imports     = filter isSupported allImports'

      -- We then create a map from import aliases (stemming from
      -- a qualified import) to their respective module path.
      qualifiedNamesAST   = map getImportQualName imports
      qualifiedNames      = map getAlias qualifiedNamesAST
      importNames         = map getImportName imports
      qualToImportNameMap = zip qualifiedNames importNames
      qualToImportNameMap' = filter (\(a,b) -> a /= "") qualToImportNameMap
      printableImports    = map prettyPrint imports
      in (printableImports, qualToImportNameMap', importNames)

   where getAlias (Just (ModuleName _ mName)) = mName
         getAlias Nothing                     = ""

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GHCI utils


callB :: Stream -> String -> IO ()
callB a b = return ()

-- | 'getGHCImoduleName' takes in a function name and returns
-- the library path where that identifier is defined.
-- TODO: parametrize the file
-- TODO: figure out how to remove its printing to stdout
-- TODO: clean its body
getGHCImoduleName :: String -> String -> IO (String, String)
getGHCImoduleName fname name =  do
  (ghciProcess, _) <- startGhci "ghci -ignore-dot-ghci" Nothing $ callB
  _      <- exec ghciProcess (":load " ++ fname)
  info' <- exec ghciProcess (":info " ++ name)
  let info = info' !! (length info' -1)
  --print info
  let path = firstLast $ strip $ (splitOn "-- Defined in" info) !! 1
  let result = if ("-- Defined in" `isInfixOf` info)  then (name, path) else (name, "")
  return result
  where firstLast xs@(_:_) = tail (init xs); firstLast _ = []

getGHCIType fname name =  do
  (ghciProcess, _) <- startGhci "ghci -ignore-dot-ghci" Nothing $ callB
  _      <- exec ghciProcess (":load " ++ fname)
  typeInfo <- (exec ghciProcess (":type " ++ name))
  let info = typeInfo !! ((length typeInfo) -1)
  let path = trace (show (splitOn ":" (info))) (strip $ (splitOn ":" (info)) !! 2)
  return path
  where firstLast xs@(_:_) = tail (init xs); firstLast _ = []


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MiscUtils.hs

-- | 'strip' takes away the whitespace around a string
-- TODO: explain why this works.
strip :: String -> String
strip  = T.unpack . T.strip . T.pack

-- | The 'splitLast' function takes as input a delimeter
-- symbol and a list. It then splits all elements in the
-- list by the last occurrence of the delimeter. The two
-- halves is returned as a tuple. If the delimeter does
-- not occurr, the element is left unchanged.
-- TODO: replace one-letter vars
splitLast :: Eq a => a -> [a] -> Either [a] ([a],[a])
splitLast c' = foldr go (Left [])
    where
        go c (Right (f,b)) = Right (c:f,b)
        go c (Left s) | c' == c = Right ([],s)
                      | otherwise = Left (c:s)

-- | The 'merge' function takes in two lists of the same type.
-- It concatenates the two lists.
-- TODO: replate usage w/ 'concat'
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys
