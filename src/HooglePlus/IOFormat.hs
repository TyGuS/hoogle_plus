module HooglePlus.IOFormat
  ( -- * types
    TypeQuery
  , QueryType(..)
  , QueryInput(..)
  , FunctionDoc(..)
  , ResultEntry(..)
  , ExecInput(..)
  , ExecOutput(..)
  , ListOutput(..)
  , QueryOutput(..)
  , ExamplesInput(..)

    -- * encoding and decoding
  , decodeInput
  , encodeOutput
  , encodeWithPrefix
  , searchExamples
  , searchResults
  , readBuiltinData
  , printResult
  , parseQueryType

    -- * example execution
  , checkExampleOutput

    -- * other
  , toOutput
  ) where

import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad                  ( unless
                                                , zipWithM
                                                )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( evalState
                                                , evalStateT
                                                )
import qualified Data.Aeson                    as A
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as LB
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as LB
import           Data.Data                      ( Data )
import           Data.List                      ( (\\)
                                                , intercalate
                                                , isInfixOf
                                                , isPrefixOf
                                                )
import           Data.List.Extra                ( dropEnd
                                                , splitOn
                                                )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( catMaybes )
import qualified Data.Serialize                as S
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Internal.Builder    as TL
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL
import           GHC.Generics                   ( Generic )
import qualified Language.Haskell.Interpreter  as LHI
import           System.Directory               ( doesFileExist )
import           Text.Parsec                    ( runParser )
import           Text.Parsec.Pos                ( initialPos )
import           Text.Printf                    ( printf )

import           Control.DeepSeq                ( NFData )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Hoogle
import           Text.PrettyPrint.ANSI.Leijen   ( string )

import           Compiler.Parser
import           Compiler.Resolver
import           Database.Dataset
import           Examples.Utils
import           HooglePlus.FilterTest
import           HooglePlus.Utils
import           Types.Common
import           Types.Environment
import           Types.Experiments
import           Types.Filtering
import           Types.Generate
import           Types.Pretty
import           Types.Program
import           Types.Type
import           Utility.Utils


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

type TypeQuery = String

data QueryInput = QueryInput
  { query      :: TypeQuery
  , inExamples :: [Example]
  , inArgNames :: [Id]
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
  Just i  -> i
  Nothing -> error "error parsing input json string"
  where mbInput = A.decode bs :: Maybe QueryInput

-- output the result into a json string
-- includes: solutions, each solution is an ast
-- accompanied with several examples
encodeOutput :: QueryOutput -> ByteString
encodeOutput = A.encode

instance Pretty QueryOutput where
  pretty = pretty . show . encodeOutput

type BuiltinExamples = Map TypeSkeleton [Example]

readBuiltinData :: Environment -> IO BuiltinExamples
readBuiltinData env = do
  let jsonPathIn = defaultJsonPath
  doesExist <- doesFileExist jsonPathIn
  unless doesExist (error "cannot find builtin json file")
  json <- readFile jsonPathIn
  let mbBuildObjs = A.decode (LB.pack json) :: Maybe [QueryInput]
  case mbBuildObjs of
    Just buildObjs -> do
      let candObjs = map transformObj buildObjs
      let candMap  = Map.fromList candObjs
      return candMap
    Nothing ->
      error "Invalid format of builtin queries, should be in json format"
  where transformObj (QueryInput q exs _) = (parseQueryType q, exs)

parseQueryType :: String -> TypeSkeleton
parseQueryType str =
  let parseResult = runParser parseType () "" str
      resolveResult t =
        runExcept $ evalStateT (resolveType [] t) initResolverState
  in  case parseResult of
        Left  parseErr -> error "something wrong in the builtin json"
        Right t        -> case resolveResult t of
          Left  err -> error $ "resolve fails" ++ show err ++ " type " ++ show t
          Right s   -> s

prepareEnvFromInput
  :: String -> IO (TypeQuery, [String], String, BuiltinExamples)
prepareEnvFromInput inStr = do
  let mbInput = A.decode (LB.pack inStr) :: Maybe ExecInput
  let input = case mbInput of
        Just i  -> i
        Nothing -> error "cannot parse the input json string"
  let args   = execArgs input
  let prog   = execProg input
  -- TODO: maybe we need to do a type checking before execution
  -- but it will also be shown in the results
  let tquery = execQuery input
  let env    = loadEnv
  buildinData <- readBuiltinData env
  return (tquery, args, prog, buildinData)

execExample
  :: [Id]
  -> Environment
  -> TypeQuery
  -> String
  -> Example
  -> IO (Either GHCError String)
execExample mdls env typ prog ex = do
  let args       = map fst (getArguments env)
  let nontcArgs = filter (not . (tyclassArgBase `Text.isPrefixOf`)) args
  let prependArg = Text.unwords nontcArgs
  let progBody = if null args -- if this is a request from front end
        then printf "let internal__f = (%s) :: %s in" prog typ
        else printf "let internal__f = (\\%s -> %s) :: %s in"
                    prependArg
                    prog
                    typ
  let parensedInputs = map wrapParens $ inputs ex
  let progCall       = printf "internal__f %s" (unwords parensedInputs)
  result <- handleApproxShowFailure mdls progBody progCall
  let prettyShow a = if "_|_" `isInfixOf` a then "bottom" else a
  return (result >>= Right . prettyShow)

checkExampleOutput
  :: [Id]
  -> Environment
  -> TypeQuery
  -> String
  -> [Example]
  -> IO (Maybe [Example])
checkExampleOutput mdls env typ prog exs = do
  let progWithoutTc = removeTypeclasses prog
  currOutputs <- mapM (execExample mdls env typ progWithoutTc) exs
  cmpResults  <- zipWithM compareResults currOutputs exs
  let justResults = catMaybes cmpResults
  if length justResults == length exs
    then return $ Just justResults
    else return Nothing
 where
  compareResults currOutput ex
    | output ex == "??" = return
    $ Just (ex { output = either id id currOutput })
    | otherwise = case currOutput of
      Left  e -> print e >> return Nothing
      Right o -> do
        expectedOutput <- handleApproxShowFailure mdls "" (output ex)
        case expectedOutput of
          Left err -> print err >> return Nothing
          Right out | o == out  -> return (Just ex)
                    | otherwise -> return Nothing

handleApproxShowFailure
  :: [Id] -> String -> String -> IO (Either GHCError String)
handleApproxShowFailure mdls progBody progCall = do
  let withApproxCall =
        printf "Test.ChasingBottoms.approxShow 100 (%s)" progCall
  result <- runStmt mdls $ unwords [progBody, withApproxCall]
  case result of
    Left  err -> runStmt mdls $ unwords [progBody, progCall]
    Right r   -> return result

searchResults :: String -> IO ()
searchResults inStr = do
  (tquery, args, prog, _) <- prepareEnvFromInput inStr
  -- first parse type query and get rid of the arg names from the signature
  let goalTyp    = parseQueryType tquery
  let goalTypStr = mkFunctionSigStr (breakdown goalTyp)
  let env        = loadEnv
  execResult <- catch
    (execExample includedModules env goalTypStr prog (Example args "??"))
    (\(e :: SomeException) -> return $ Left (show e))
  let execJson = case execResult of
        Left  err -> ExecOutput err ""
        Right r   -> ExecOutput "" r
  printResult $ encodeWithPrefix execJson

searchExamples :: [Id] -> String -> Int -> IO ()
searchExamples mdls inStr num = do
  let mbInput = A.decode (LB.pack inStr) :: Maybe ExamplesInput
  let input = case mbInput of
        Just i  -> i
        Nothing -> error "cannot parse the input json string"
  let exists     = map inputs $ exampleExisting input
  let prog       = exampleProgram input
  -- TODO: maybe we need to do a type checking before execution
  -- but it will also be shown in the results
  let namedQuery = exampleQuery input

  let env        = loadEnv
  candMap <- readBuiltinData env
  let goalTyp      = parseQueryType namedQuery
  let unnamedQuery = show goalTyp
  let funcSig      = parseTypeString unnamedQuery
  let argNames     = words (getArgNames prog)
  -- remove magic numbers later
  -- we not only need to know existing results, since this should come from
  -- the diverse stream, we also need to pass all the current solutions into
  -- this query
  let prevResults  = map output $ exampleExisting input
  let prevInputs = map (map toNormalFunctions . inputs) $ exampleExisting input
  result <- generateIOPairs mdls
                            prog
                            funcSig
                            argNames
                            num
                            defaultTimeoutMicro
                            defaultGenerationTimeoutMicro
                            defaultGenerationDepth
                            prevResults
                            prevInputs
  resultObj <- case result of
    Left err -> return $ ListOutput [] (show err)
    Right genRes
      | null genRes -> return $ ListOutput [] "No more examples"
      | otherwise -> do
        niceExamples <- mapM niceInputs genRes
        return $ ListOutput niceExamples ""
  printResult $ encodeWithPrefix resultObj
 where
  getArgNames str = getArgNames' "" str

  getArgNames' _   ('\\'          : str) = getArgNames' "" str
  getArgNames' acc (c : '-' : '>' : _  ) = reverse (c : acc)
  getArgNames' acc (c             : str) = getArgNames' (c : acc) str
  getArgNames' _ _ = error "getArgNames: not a valid arg name"

  toNormalFunctions "" = ""
  toNormalFunctions f | "const " `isPrefixOf` f =
    let n = drop 6 f
    in  printf "(\\x -> case x of 0 -> %s; 1 -> %s; -1 -> %s; 2 -> %s; ...)"
               n
               n
               n
               n
  toNormalFunctions "\\x -> x + 1" =
    "(\\x -> case x of 0 -> 1; 1 -> 2; -1 -> 0; 2 -> 3; ...)"
  toNormalFunctions "\\x -> x * x" =
    "(\\x -> case x of 0 -> 0; 1 -> 1; -1 -> 1; 2 -> 4; ...)"
  toNormalFunctions "\\x -> x * 3" =
    "(\\x -> case x of 0 -> 0; 1 -> 3; -1 -> -3; 2 -> 6; ...)"
  toNormalFunctions f | head f == '[' && last f == ']' =
    let elmts     = dropEnd 1 $ drop 1 f
        sepElmts  = splitOn "," elmts
        convElmts = map toNormalFunctions sepElmts
    in  printf "[%s]" (intercalate "," convElmts)
  toNormalFunctions f = f


encodeWithPrefix :: A.ToJSON a => a -> LB.ByteString
encodeWithPrefix obj = LB.append
  (LB.toLazyByteString $ TL.encodeUtf8Builder $ TL.fromStrict outputPrefix)
  (A.encode obj)

printResult :: LB.ByteString -> IO ()
printResult bs = putStrLn (LB.unpack bs)

toOutput :: Environment -> TProgram -> AssociativeExamples -> IO QueryOutput
toOutput env soln exs = do
  let symbols       = Set.toList $ symbolsOf soln
  let args          = getArguments env
  let argNames      = map fst args
  let argDocs = map (\(n, ty) -> FunctionDoc (Text.unpack n) (show ty) "") args
  let symbolsWoArgs = symbols \\ argNames
  entries <- mapM mkEntry exs
  return $ QueryOutput entries "" argDocs
 where
  mkEntry ((unqualSol, qualSol), ex) = do
    ex' <- mapM niceInputs ex
    let qualSol'   = toHaskellSolution $ show qualSol
    let unqualSol' = toHaskellSolution $ show unqualSol
    return (ResultEntry qualSol' unqualSol' ex')
  hoogleIt syms = do
    dbPath <- Hoogle.defaultDatabaseLocation
    Hoogle.withDatabase
      dbPath
      (\db -> do
        let targets = map (head . Hoogle.searchDatabase db) syms
        let docs    = map targetToDoc targets
        return docs
      )

  targetToDoc tg =
    let wholeSig = unHTML $ Hoogle.targetItem tg
        segs     = splitOn " :: " wholeSig
        name     = head segs
        sig      = unwords $ tail segs
        doc      = unHTML $ Hoogle.targetDocs tg
    in  FunctionDoc name sig doc
