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
    printResult
    ) where

import Types.IOFormat
import Types.Experiments
import Types.Environment
import Types.Type
import Synquid.Parser (parseType, parseProgram)
import Synquid.Resolver (ResolverState(..), initResolverState, resolveSchema)
import Synquid.Type
import Examples.ExampleChecker
import Examples.InferenceDriver
import Examples.Utils

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Except (runExcept)
import Control.Monad.State (evalState, evalStateT)
import Control.Exception
import Control.Lens
import System.Directory
import System.FilePath
import Data.List
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
        transformObj (QueryInput q exs) = (parseQueryType env q, exs)

parseQueryType :: Environment -> String -> RSchema
parseQueryType env str = let
    parseResult = flip evalState (initialPos "type") $ 
                  runIndentParserT parseType () "" str
    resolveResult t = runExcept $ evalStateT (resolveSchema t) $
                      initResolverState { _environment = env }
    in case parseResult of
           Left parseErr -> error "something wrong in the builtin json"
           Right t -> case resolveResult (Monotype t) of
                          Left err -> error "resolve fails for the buildin json"
                          Right s -> s

searchTypes :: SynquidParams -> String -> IO ()
searchTypes synquidParams inStr = do
    let input = decodeInput (LB.pack inStr)
    let exquery = inExamples input
    env <- readEnv $ envPath synquidParams
    let mdls = Set.toList $ env ^. included_modules
    let mkFun ex = printf "(%s)" (intercalate ", " $ inputs ex ++ [output ex])
    exTypes <- mapM (parseExample mdls . mkFun) exquery
    let (validSchemas, invalidTypes) = partitionEithers exTypes
    resultObj <- if null invalidTypes then possibleQueries env exquery validSchemas
                                      else return $ ListOutput [] (unlines invalidTypes)
    printResult $ encodeWithPrefix resultObj
    where
        renameVars t = 
            let freeVars = Set.toList $ typeVarsOf t
                validVars = foldr delete seqChars freeVars
                substVars = foldr delete freeVars seqChars
                substMap = Map.fromList $ zip substVars $ map vart_ validVars
             in stypeSubstitute substMap t

        possibleQueries env exquery exTypes = do
            generalTypes <- getExampleTypes env exTypes
            if null generalTypes then return $ ListOutput [] "Cannot find type for your query"
                                 else return $ ListOutput generalTypes ""

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

searchExamples :: SynquidParams -> String -> IO ()
searchExamples synquidParams inStr = do
    let mbInput = A.decode (LB.pack inStr) :: Maybe ExamplesInput
    let input = case mbInput of
                    Just i -> i
                    Nothing -> error "cannot parse the input json string"
    let exists = map inputs $ exampleExisting input
    let prog = exampleProgram input
    -- TODO: maybe we need to do a type checking before execution
    -- but it will also be shown in the results
    let strQuery = exampleQuery input
    env <- readEnv $ envPath synquidParams
    env' <- readBuiltinData synquidParams env
    let mdls = Set.toList $ env' ^. included_modules
    let candMap = env' ^. queryCandidates
    let builtinQueryTypes = Map.keys candMap 
    let tQuery = parseQueryType env' strQuery
    messageChan <- newChan
    outRes <- mapM (checkTypes env' messageChan tQuery) builtinQueryTypes
    let outQueries = map snd $ filter (fst . fst) $ zip outRes builtinQueryTypes
    let examples = concatMap (candMap Map.!) outQueries
    let checkDups ex = (inputs ex) `notElem` exists
    let uniqueExs = filter checkDups examples
    mbOutputs <- checkExampleOutput mdls env' strQuery prog uniqueExs
    let resultObj = if null uniqueExs then ListOutput [] "No more examples"
                                      else ListOutput (fromJust mbOutputs) ""
    printResult $ encodeWithPrefix resultObj

searchResults :: SynquidParams -> String -> IO ()
searchResults synquidParams inStr = do
    (tquery, args, prog, env) <- prepareEnvFromInput synquidParams inStr
    let mdls = Set.toList $ env ^. included_modules
    execResult <- catch (execExample mdls env tquery prog (Example args "??"))
                        (\(e :: SomeException) -> return $ Left (show e))
    let execJson = case execResult of
                        Left err -> ExecOutput err ""
                        Right r -> ExecOutput "" r
    printResult $ encodeWithPrefix execJson

encodeWithPrefix :: A.ToJSON a => a -> LB.ByteString
encodeWithPrefix obj = LB.append (LB.pack outputPrefix) (A.encode obj)

printResult :: LB.ByteString -> IO ()
printResult bs = putStrLn (LB.unpack bs)
