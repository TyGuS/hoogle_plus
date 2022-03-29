module HooglePlus.IOFormat(
    decodeInput,
    encodeOutput,
    encodeWithPrefix,
    searchExamples,
    readEnv,
    readBuiltinData,
    printResult,
    parseQueryType
    ) where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Except (runExcept)
import Control.Monad.State (evalState, evalStateT)
import Control.Exception
import Control.Lens
import System.Directory
import System.FilePath
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Either
import qualified Data.Serialize as S
import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Builder as LB
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec.Indent
import Text.Parsec.Pos
import Text.Printf
import qualified Language.Haskell.Interpreter as LHI
import Data.Text ( Text )
import qualified Data.Text as Text
import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Map ( Map )

import Control.DeepSeq ( NFData )
import Data.Aeson ( FromJSON, ToJSON )
import Hoogle
import Text.PrettyPrint.ANSI.Leijen ( string )

import Types.Experiments
import Types.Environment
import Types.Type
import Types.Program
import Types.Filtering
import Types.Pretty
import HooglePlus.FilterTest
import HooglePlus.Utils
import Utility.Utils
import Compiler.Parser
import Compiler.Resolver
import Types.Common
import qualified Data.Text.Internal.Builder as TL


--------------------------------------------------------------------------------
--------------------------- Command Line Input Format --------------------------
--------------------------------------------------------------------------------

outputPrefix :: Text
outputPrefix = "RESULTS:"

data QueryType = SearchPrograms
               | SearchTypes
               | SearchResults
               | SearchExamples
  deriving(Eq, Data, Show, Generic)

instance FromJSON QueryType
instance ToJSON QueryType

type ErrorMessage = String
type TypeQuery = String

data QueryInput = QueryInput
  { query      :: TypeQuery
  , inExamples :: [Example]
  , inArgNames :: [String]
  }
  deriving (Eq, Show, Generic)

instance FromJSON QueryInput
instance ToJSON QueryInput

data FunctionDoc = FunctionDoc
  { functionName :: String
  , functionSig  :: String
  , functionDesc :: String
  }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON FunctionDoc
instance Pretty FunctionDoc where
  pretty (FunctionDoc n sig desc) = string $ unlines
    ["Function: " ++ n, "Signature: " ++ sig, "Description: " ++ desc]

data ResultEntry = ResultEntry
  { qualSolution   :: String
  , unqualSolution :: String
  , outExamples    :: [Example]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ResultEntry

data QueryOutput = QueryOutput
  { outCandidates :: [ResultEntry]
  , outError      :: String
  , outDocs       :: [FunctionDoc]
  }
  deriving (Eq, Show, Generic)

instance ToJSON QueryOutput

data ExecInput = ExecInput
  { execQuery :: TypeQuery
  , execArgs  :: [String]
  , execProg  :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON ExecInput

data ExecOutput = ExecOutput
  { execError  :: String
  , execResult :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON ExecOutput

data ExamplesInput = ExamplesInput
  { exampleQuery    :: TypeQuery
  , exampleProgram  :: String
  , exampleExisting :: [Example]
  }
  deriving (Eq, Show, Generic)

instance FromJSON ExamplesInput

data ListOutput a = ListOutput
  { examplesOrTypes :: [a]
  , tqError         :: String
  }
  deriving (Eq, Generic)

instance ToJSON a => ToJSON (ListOutput a)

--------------------------------------------------------------------------------
--------------------------- IO Format Parsing ----------------------------------
--------------------------------------------------------------------------------

-- parse the input json string
-- includes: type query, examples
decodeInput :: ByteString -> QueryInput
decodeInput bs = case mbInput of
                   Just i -> i
                   Nothing -> error "error parsing input json string"
    where
        mbInput = A.decode bs :: Maybe QueryInput

-- output the result into a json string
-- includes: solutions, each solution is an ast
-- accompanied with several examples
encodeOutput :: QueryOutput -> ByteString
encodeOutput = A.encode

instance Pretty QueryOutput where
    pretty = pretty . show . encodeOutput

-- | TODO: redesign this part
readEnv :: FilePath -> IO Environment
readEnv envPathIn = do
    doesExist <- doesFileExist envPathIn
    unless doesExist (error "Please run `stack exec -- hplus generate -p [PACKAGES]` to generate database first")
    envRes <- S.decode <$> B.readFile envPathIn
    case envRes of
        Left err -> error err
        Right env -> return env

type BuiltinExamples = Map SchemaSkeleton [Example]

readBuiltinData :: SynquidParams -> Environment -> IO BuiltinExamples
readBuiltinData synquidParams env = do
    let jsonPathIn = jsonPath synquidParams
    doesExist <- doesFileExist jsonPathIn
    unless doesExist (error "cannot find builtin json file")
    json <- readFile jsonPathIn
    let mbBuildObjs = A.decode (LB.pack json) :: Maybe [QueryInput]
    case mbBuildObjs of
        Just buildObjs -> do
            let candObjs = map transformObj buildObjs
            let candMap = Map.fromList candObjs
            return candMap
        Nothing -> error "Invalid format of builtin queries, should be in json format"
    where
        transformObj (QueryInput q exs _) = (parseQueryType env q, exs)

parseQueryType :: Environment -> String -> SchemaSkeleton
parseQueryType env str = let
    parseResult = flip evalState (initialPos "type") $
                  runIndentParserT parseSchema () "" str
    resolveResult t = runExcept $ evalStateT (resolveSchema (getBoundTypeVars env) t) initResolverState
    in case parseResult of
           Left parseErr -> error "something wrong in the builtin json"
           Right t -> case resolveResult t of
                          Left err -> error $ "resolve fails" ++ show err ++ " type " ++ show t
                          Right s -> s

prepareEnvFromInput :: SynquidParams -> String -> IO (TypeQuery, [String], String, BuiltinExamples)
prepareEnvFromInput synquidParams inStr = do
    let mbInput = A.decode (LB.pack inStr) :: Maybe ExecInput
    let input = case mbInput of
                    Just i -> i
                    Nothing -> error "cannot parse the input json string"
    let args = execArgs input
    let prog = execProg input
    -- TODO: maybe we need to do a type checking before execution
    -- but it will also be shown in the results
    let tquery = execQuery input
    env <- readEnv $ envPath synquidParams
    buildinData <- readBuiltinData synquidParams env
    return (tquery, args, prog, buildinData)

searchExamples :: SynquidParams -> [Id] -> String -> Int -> IO ()
searchExamples synquidParams mdls inStr num = do
    let mbInput = A.decode (LB.pack inStr) :: Maybe ExamplesInput
    let input = case mbInput of
                    Just i -> i
                    Nothing -> error "cannot parse the input json string"
    let exists = map inputs $ exampleExisting input
    let prog = exampleProgram input
    -- TODO: maybe we need to do a type checking before execution
    -- but it will also be shown in the results
    let namedQuery = exampleQuery input

    env <- readEnv $ envPath synquidParams
    candMap <- readBuiltinData synquidParams env
    let goalTyp = parseQueryType env namedQuery
    let unnamedQuery = show (toMonotype goalTyp)
    let funcSig = parseTypeString unnamedQuery
    let argNames = words (getArgNames prog)
    -- remove magic numbers later
    -- we not only need to know existing results, since this should come from
    -- the diverse stream, we also need to pass all the current solutions into
    -- this query
    let prevResults = map output $ exampleExisting input
    let prevInputs = map (map toNormalFunctions . inputs) $ exampleExisting input
    result <- generateIOPairs mdls prog funcSig argNames num defaultTimeoutMicro defaultGenerationTimeoutMicro defaultGenerationDepth prevResults prevInputs
    resultObj <- case result of
          Left err -> return $ ListOutput [] (show err)
          Right genRes | null genRes -> return $ ListOutput [] "No more examples"
                       | otherwise -> do
                            niceExamples <- mapM niceInputs genRes
                            return $ ListOutput niceExamples ""
    printResult $ encodeWithPrefix resultObj
    where
        getArgNames str = getArgNames' "" str
        getArgNames' _ ('\\':str) = getArgNames' "" str
        getArgNames' acc (c:'-':'>':_) = reverse (c:acc)
        getArgNames' acc (c:str) = getArgNames' (c:acc) str

        toNormalFunctions "" = ""
        toNormalFunctions f | "const " `isPrefixOf` f =
            let n = drop 6 f
             in printf "(\\x -> case x of 0 -> %s; 1 -> %s; -1 -> %s; 2 -> %s; ...)" n n n n
        toNormalFunctions "\\x -> x + 1" = "(\\x -> case x of 0 -> 1; 1 -> 2; -1 -> 0; 2 -> 3; ...)"
        toNormalFunctions "\\x -> x * x" = "(\\x -> case x of 0 -> 0; 1 -> 1; -1 -> 1; 2 -> 4; ...)"
        toNormalFunctions "\\x -> x * 3" = "(\\x -> case x of 0 -> 0; 1 -> 3; -1 -> -3; 2 -> 6; ...)"
        toNormalFunctions f | head f == '[' && last f == ']' =
            let elmts = dropEnd 1 $ drop 1 f
                sepElmts = splitOn "," elmts
                convElmts = map toNormalFunctions sepElmts
             in printf "[%s]" (intercalate "," convElmts)
        toNormalFunctions f = f


encodeWithPrefix :: A.ToJSON a => a -> LB.ByteString
encodeWithPrefix obj = LB.append (LB.toLazyByteString $ TL.encodeUtf8Builder $ TL.fromStrict outputPrefix) (A.encode obj)

printResult :: LB.ByteString -> IO ()
printResult bs = putStrLn (LB.unpack bs)

toOutput :: Environment -> TProgram -> AssociativeExamples -> IO QueryOutput
toOutput env soln exs = do
    let symbols = Set.toList $ symbolsOf soln
    let args = getArguments env
    let argNames = map fst args
    let argDocs = map (\(n, ty) -> FunctionDoc (Text.unpack n) (show ty) "") args
    let symbolsWoArgs = symbols \\ argNames
    entries <- mapM mkEntry exs
    return $ QueryOutput entries "" argDocs
    where
        mkEntry ((unqualSol, qualSol), ex) = do
            ex' <- mapM niceInputs ex
            let qualSol' = toHaskellSolution $ show qualSol
            let unqualSol' = toHaskellSolution $ show unqualSol
            return (ResultEntry qualSol' unqualSol' ex')
        hoogleIt syms = do
            dbPath <- Hoogle.defaultDatabaseLocation
            Hoogle.withDatabase dbPath (\db -> do
                let targets = map (head . Hoogle.searchDatabase db) syms
                let docs = map targetToDoc targets
                return docs)

        targetToDoc tg = let wholeSig = unHTML $ Hoogle.targetItem tg
                             segs = splitOn " :: " wholeSig
                             name = head segs
                             sig = unwords $ tail segs
                             doc = unHTML $ Hoogle.targetDocs tg
                          in FunctionDoc name sig doc