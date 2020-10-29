{-# LANGUAGE ScopedTypeVariables #-}

module HooglePlus.IOFormat(
    decodeInput,
    encodeOutput,
    encodeWithPrefix,
    searchTypes,
    searchResults,
    searchExamples,
    readEnv,
    readBuiltinData,
    printResult,
    parseQueryType
    ) where

import Types.IOFormat
import Types.InfConstraint (InfStats(..))
import Types.Experiments
import Types.Environment
import Types.Type
import Types.Filtering (defaultTimeoutMicro, defaultGenerationDepth, defaultInterpreterTimeoutMicro, defaultGenerationTimeoutMicro)
import Synquid.Parser (parseSchema, parseProgram)
import Synquid.Resolver (ResolverState(..), initResolverState, resolveSchema)
import Synquid.Type
import Synquid.Pretty
import Examples.ExampleChecker
import Examples.InferenceDriver
import Examples.Utils
import HooglePlus.FilterTest (generateIOPairs, parseTypeString)
import HooglePlus.Utils (niceInputs)

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
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec.Indent
import Text.Parsec.Pos
import Text.Printf
import qualified Language.Haskell.Interpreter as LHI

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
encodeOutput out = A.encode out

instance Show QueryOutput where
    show = show . encodeOutput 

readEnv :: FilePath -> IO Environment
readEnv envPathIn = do
    doesExist <- doesFileExist envPathIn
    when (not doesExist) (error ("Please run `stack exec -- hplus generate -p [PACKAGES]` to generate database first"))
    envRes <- S.decode <$> B.readFile envPathIn
    case envRes of
        Left err -> error err
        Right env -> return env

readBuiltinData :: SynquidParams -> Environment -> IO Environment
readBuiltinData synquidParams env = do
    let jsonPathIn = jsonPath synquidParams
    doesExist <- doesFileExist jsonPathIn
    when (not doesExist) (error "cannot find builtin json file")
    json <- readFile jsonPathIn
    let mbBuildObjs = A.decode (LB.pack json) :: Maybe [QueryInput]
    case mbBuildObjs of
        Just buildObjs -> do
            let candObjs = map transformObj buildObjs
            let candMap = Map.fromList candObjs
            return $ env {_queryCandidates = candMap}
        Nothing -> error "Invalid format of builtin queries, should be in json format"
    where
        transformObj (QueryInput q exs _) = (parseQueryType env q, exs)

parseQueryType :: Environment -> String -> RSchema
parseQueryType env str = let
    parseResult = flip evalState (initialPos "type") $ 
                  runIndentParserT parseSchema () "" str
    resolveResult t = runExcept $ evalStateT (resolveSchema t) $
                      initResolverState { _environment = env }
    in case parseResult of
           Left parseErr -> error "something wrong in the builtin json"
           Right t -> case resolveResult t of
                          Left err -> error $ "resolve fails" ++ show err ++ " type " ++ show t
                          Right s -> s

searchTypes :: SynquidParams -> String -> Int -> IO (ListOutput String, InfStats)
searchTypes synquidParams inStr num = do
    let input = decodeInput (LB.pack inStr)
    let exquery = inExamples input
    env <- readEnv $ envPath synquidParams
    let mdls = Set.toList $ env ^. included_modules
    let mkFun ex = printf "(%s)" (intercalate ", " $ inputs ex ++ [output ex])
    exTypes <- mapM (parseExample mdls . mkFun) exquery
    let (invalidTypes, validSchemas) = partitionEithers exTypes
    let argNames = inArgNames input
    resultObj <- if null invalidTypes then possibleQueries env argNames exquery validSchemas
                                      else return $ (ListOutput [] (unlines invalidTypes), InfStats (-1) (-1))
    printResult $ encodeWithPrefix $ fst resultObj
    return resultObj
    where
        renameVars t = 
            let freeVars = Set.toList $ typeVarsOf t
                validVars = foldr delete seqChars freeVars
                substVars = foldr delete freeVars seqChars
                substMap = Map.fromList $ zip substVars $ map vart_ validVars
             in stypeSubstitute substMap t

        possibleQueries env argNames exquery exTypes = do
            (generalTypes, stats) <- getExampleTypes env argNames exTypes num
            if null generalTypes then return $ (ListOutput [] "Cannot find type for your query", InfStats 0 0)
                                 else return $ (ListOutput generalTypes "", stats)

prepareEnvFromInput :: SynquidParams -> String -> IO (TypeQuery, [String], String, Environment)
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
    env' <- readBuiltinData synquidParams env
    return (tquery, args, prog, env')

searchExamples :: SynquidParams -> String -> Int -> IO ()
searchExamples synquidParams inStr num = do
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
    env' <- readBuiltinData synquidParams env
    let mdls = Set.toList $ env' ^. included_modules
    let candMap = env' ^. queryCandidates

    let goalTyp = parseQueryType env' namedQuery
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

searchResults :: SynquidParams -> String -> IO ()
searchResults synquidParams inStr = do
    (tquery, args, prog, env) <- prepareEnvFromInput synquidParams inStr
    let mdls = Set.toList $ env ^. included_modules
    -- first parse type query and get rid of the arg names from the signature
    let goalTyp = parseQueryType env tquery
    let goalTypStr = show (toMonotype goalTyp)
    execResult <- catch (execExample mdls env goalTypStr prog (Example args "??"))
                        (\(e :: SomeException) -> return $ Left (show e))
    let execJson = case execResult of
                        Left err -> ExecOutput err ""
                        Right r -> ExecOutput "" r
    printResult $ encodeWithPrefix execJson

encodeWithPrefix :: A.ToJSON a => a -> LB.ByteString
encodeWithPrefix obj = LB.append (LB.pack outputPrefix) (A.encode obj)

printResult :: LB.ByteString -> IO ()
printResult bs = putStrLn (LB.unpack bs)
