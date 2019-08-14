module HooglePlus.FilterTest (runChecks, checkSolutionNotCrash, checkDuplicates) where

import Language.Haskell.Interpreter hiding (get)
import Language.Haskell.Exts.Parser
import Text.Printf
import Control.Exception
import Data.List
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Test.QuickCheck
import qualified Data.Map as Map hiding (map, foldr)
import qualified Data.Set as Set hiding (map)
import System.Timeout
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Typeable

import HooglePlus.Utils
import Types.Environment
import Types.Program
import Types.Type hiding (typeOf)
import Types.Filtering
import Synquid.Type

parseTypeString :: String -> FunctionSignature
parseTypeString input = FunctionSignature constraints argsType returnType
  where
    (constraints, argsType, returnType) = buildSig [] [] value
    (ParseOk value) = parseType input

    buildSig constraints argList (TyForall _ _ (Just ctx) t) = buildSig constraints' argList t
      where constraints' = constraints ++ extractConstraints constraints ctx
    buildSig constraints argList (TyFun _ typeArg typeRet) = buildSig constraints argList' typeRet
      where argList' = argList ++ [extractType typeArg]
    buildSig constraints argList typeRet = (constraints, argList, extractType typeRet)

    extractType (TyVar _ (Ident _ name)) = Polymorphic name
    extractType (TyCon _ (UnQual _ (Ident _ name))) = Concrete name
    extractType (TyCon _ (Qual _ (ModuleName _ moduleName) (Ident _ id))) = Concrete (printf "%s.%s" moduleName id)
    extractType (TyList _ arg) = ArgTypeList (extractType arg)
    extractType (TyParen _ t) = extractType t
    extractType (TyApp _ l r) = ArgTypeApp (extractType l) (extractType r)
    extractType (TyTuple _ _ types) = ArgTypeTuple (map extractType types)
    extractType other = throw $ NotSupportedException ("Not able to handle " ++ show other)

    extractQualified (ClassA _ (UnQual _ (Ident _ name)) var) =
      map (\x -> TypeConstraint (show $ extractType x) name) var
    extractQualified (ParenA _ qual) = extractQualified qual
    extractQualified other = throw $ NotSupportedException ("Not able to extract " ++ show other)

    extractConstraints constraints (CxSingle _ item) = constraints ++ extractQualified item
    extractConstraints constraints (CxTuple _ list) = foldr ((++) . extractQualified) constraints list

-- instantiate polymorphic types in function signature with `Int`
-- * note that constraints were ignored since we only use `Int` now
instantiateSignature :: FunctionSignature -> FunctionSignature
instantiateSignature (FunctionSignature _ argsType returnType) =
  FunctionSignature [] (map instantiate argsType) (instantiate returnType)
    where
      instantiate (Concrete name) = Concrete name
      instantiate (Polymorphic name) = Concrete "Int"
      instantiate (ArgTypeList sub) = ArgTypeList $ instantiate sub
      instantiate (ArgTypeTuple types) = ArgTypeTuple (map instantiate types)
      instantiate (ArgTypeApp l r) = ArgTypeApp (instantiate l) (instantiate r)

generateRandomValue :: [String] -> String -> IO String
generateRandomValue imports typeName = do
  result <- runInterpreter $ do {
    setImports ("Test.QuickCheck":imports);

    act <- interpret ("do {\
    \it <- generate (arbitrary :: Gen (" ++ typeName ++ "));\
    \return $ show it\
    \}") (as :: IO String);

    liftIO act
  }

  case result of
    Left err -> putStrLn (displayException err) >> return "error \"unable to generate input\""
    Right res -> return ("(" ++ res ++ ")")

generateRandomValueWith :: Int -> Int -> [String] -> String -> IO String
generateRandomValueWith seed size imports typeName = do
  result <- runInterpreter $ do
    setImports ("Test.QuickCheck":imports)
    eval $ printf "unGen arbitrary (mkQCGen (%d)) (%d)" seed size

  case result of
    Left err -> putStrLn (displayException err) >> return "error \"unable to generate input\""
    Right res -> return $ printf "(%s)" res


mapRandomParamsList :: [String] -> FunctionSignature -> Int -> IO [String]
mapRandomParamsList imports env count = do
  base <- generateTestInput' imports env
  replicateM count (derive base)
  where
    derive :: [(String, String)] -> IO String
    derive base = do
      rand <- generate (arbitrary :: Gen Int)
      let index = rand `rem` length base
      let item = getNthElement index base

      replaced <- newRandomValue item
      let list = setNthElement index replaced base
      return $ unwords (map snd list)

    newRandomValue (name, _) = do
      val <- generateRandomValue imports name
      return (name, val)

    getNthElement idx list = item
      where (_, item:_) = splitAt idx list

    setNthElement idx item list
      | idx < 0   = list
      | otherwise = case splitAt idx list of
                    (front, _:back) -> front ++ item : back
                    _ -> list

-- generate test input as line of code
generateTestInput :: [String] -> FunctionSignature -> IO String
generateTestInput imports env = do
  result <- generateTestInput' imports env
  return $ unwords (map snd result)

-- generate test input as pair (type, value)
generateTestInput' :: [String] -> FunctionSignature -> IO [(String, String)]
generateTestInput' imports funcSig = mapM f (_argsType funcSig)
  where
    f x = let str = show x in do
      val <- generateRandomValue imports str
      return (str, val)

eval_ :: [String] -> String -> Int -> IO (Maybe (Either InterpreterError String))
eval_ modules line time = timeout time $ runInterpreter $ do
  setImports modules
  eval line

runStmt_ :: [String] -> String -> Int -> IO (Maybe (Either InterpreterError ()))
runStmt_ modules line time = timeout time $ runInterpreter $ do
  setImports modules
  runStmt $ printf line

runChecks :: MonadIO m => Environment -> RType -> UProgram -> FilterTest m Bool
runChecks env goalType prog =
  and <$> mapM (\f -> f modules funcSig body) checks
  where
    (modules, funcSig, body, _) = extractSolution env goalType prog

    checks = [ checkSolutionNotCrash
             , checkDuplicates]

checkSolutionNotCrash :: MonadIO m => [String] -> String -> String -> FilterTest m Bool
checkSolutionNotCrash modules sigStr body = or <$> liftIO (replicateM defaultNumChecks executeCheck)
  where
    executeCheck = handleNotSupported $ do
      let funcSig = (instantiateSignature . parseTypeString) sigStr
      arg <- generateTestInput modules funcSig
      result <- handleAnyException $ runStmt_ modules (fmtFunction_ body (show funcSig) arg) defaultTimeoutMicro
      case result of
        Nothing -> putStrLn "Timeout in running always-fail detection" >> return True
        Just (Left err) -> putStrLn (displayException err) >> return False
        Just (Right ()) -> return True

handleNotSupported = (`catch` ((\ex -> print ex >> return True) :: NotSupportedException -> IO Bool))
handleAnyException = (`catch` (return . handler . show :: SomeException -> IO (Maybe (Either InterpreterError ()))))
  where
    handler ex  | "timeout" `isInfixOf` ex = Nothing
                | otherwise = (Just . Left. UnknownError) ex
fmtFunction_ = printf "((%s) :: %s) %s" :: String -> String -> String -> String

checkDuplicates :: MonadIO m => [String] -> String -> String -> FilterTest m Bool
checkDuplicates modules sigStr body = do
  st <- get
  case sampleResults st of
    Nothing -> do
      result' <- liftIO (Just . (`SampleResult` []) <$> generateInputs modules funcSig)
      modify $ \st -> st { sampleResults = result' }
      checkDuplicates modules sigStr body
    Just (SampleResult inputs results) -> do
      evals <- liftIO $ evalResults inputs funcSig body
      let isDuplicated = evals `elem` results
      let results' = if isDuplicated then results else evals:results

      modify $ \st -> st { sampleResults = Just $ SampleResult inputs results' }
      return $ not isDuplicated

  where
    funcSig = (instantiateSignature . parseTypeString) sigStr

    generateInputs :: [String] -> FunctionSignature -> IO [String]
    generateInputs modules funcSig = replicateM defaultNumChecks (generateTestInput modules funcSig)
    evalResults inputs funcSig body = mapM evalWithArg inputs

    evalWithArg arg =
        fst . splitAt defaultEvalLength . show <$> eval_ modules (fmtFunction_ body (show funcSig) arg) defaultTimeoutMicro
