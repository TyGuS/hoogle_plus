module HooglePlus.FilterTest (runChecks, checkSolutionNotCrash, checkDuplicates) where

import Language.Haskell.Interpreter hiding (get)
import Language.Haskell.Interpreter.Unsafe
import Language.Haskell.Exts.Parser
import Text.Printf
import Control.Exception
import Data.List
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import qualified Data.Map as Map hiding (map, foldr)
import qualified Data.Set as Set hiding (map)
import System.Timeout
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Typeable

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import HooglePlus.Utils
import Types.Environment
import Types.Program
import Types.Type hiding (typeOf)
import Types.Filtering
import Synquid.Type
import Paths_HooglePlus

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
    extractType (TyFun _ src dst) = ArgTypeFunc (extractType src) (extractType dst)
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
      instantiate sig@(Concrete name) = sig
      instantiate (Polymorphic name) = Concrete "Int"
      instantiate (ArgTypeList sub) = ArgTypeList $ instantiate sub
      instantiate (ArgTypeTuple types) = ArgTypeTuple (map instantiate types)
      instantiate (ArgTypeApp l r) = ArgTypeApp (instantiate l) (instantiate r)
      instantiate (ArgTypeFunc l r) = ArgTypeFunc (instantiate l) (instantiate r)

toInnerType :: FunctionSignature -> FunctionSignature
toInnerType (FunctionSignature _ argsType returnType) =
  FunctionSignature [] (map f argsType) (f returnType)
    where
      f sig@(Concrete name)
        | name `elem` supportedInnerType = Concrete (printf "(Internal (%s))" name)
        | otherwise = sig
      f (ArgTypeList sub) = ArgTypeList $ f sub
      f (ArgTypeTuple types) = ArgTypeTuple (map f types)
      f (ArgTypeApp l r) = ArgTypeApp (f l) (f r)
      f (ArgTypeFunc l r) = ArgTypeFunc (f l) (f r)

-- generate first-order values from QuickCheck
generateRandomValue :: [String] -> [String] -> IO [String]
generateRandomValue imports typeNames = do
  srcPath <- getDataFileName "InternalTypeGen.hs"
  result <- runInterpreter $ do {
    loadModules [srcPath];
    setTopLevelModules ["InternalTypeGen"];
    setImports imports;

    mapM (\x -> interpret (fmtLine x) (as :: IO String) >>= liftIO) typeNames
  }

  case result of
    Left err -> putStrLn (displayException err) >> return ["error \"unable to generate input\""]
    Right res -> return $ map (printf "(%s)") res
  where
    fmtLine typeName = printf "show <$> generate (arbitrary :: Gen (%s))" typeName :: String

-- map custom seed and size to value of arbitrary type
generateRandomValueWith :: Arbitrary a => Int -> Int -> a
generateRandomValueWith seed = unGen arbitrary (mkQCGen seed)

-- generate test input as line of code
generateTestInput :: [String] -> FunctionSignature -> IO GeneratedInput
generateTestInput imports funcSig = do

  hoArgs <- transformHOs hoArgTypes
  foArgs <- transformFOs foArgTypes

  let args = sortBy (\(l, _) (r, _) -> compare l r) (hoArgs ++ foArgs)
  return $ map snd args

  where

    argTypeIndexed :: [(Int, ArgumentType)]
    argTypeIndexed = zip [1..] (_argsType funcSig)
    hoArgTypes = filter (isHigherOrderArgument . snd) argTypeIndexed
    foArgTypes = filter (not . isHigherOrderArgument . snd) argTypeIndexed

    transformHOs hoArgTypes =
      zip indexes <$> mapM f (zip [1..] hoArgTypes)
      where
        indexes = map fst hoArgTypes
        f (index, (_, argType)) = do
          seed <- generate arbitrary :: IO Int
          size <- generate arbitrary :: IO Int
          
          let typeStr = show argType
          return (HigherOrder typeStr index seed size)

    transformFOs foArgTypes =
      let indexes = map fst foArgTypes in
      let argTypeStrs = map (show . snd) foArgTypes in
      let generatedValues = generateRandomValue imports argTypeStrs in
        zip indexes . map Value <$> generatedValues

evalHOF_ :: [String] -> [String] -> String -> Int -> IO (Maybe (Either InterpreterError String))
evalHOF_ modules inits line time = handleAnyException $ timeout time $ do
  srcPath <- getDataFileName "InternalTypeGen.hs"
  result <- unsafeRunInterpreterWithArgs ["-fno-omit-yields"] $ do

    loadModules [srcPath]
    setTopLevelModules ["InternalTypeGen"]
    setImports (quickCheckModules ++ modules)

    mapM_ runStmt inits
    eval line
  case result of
    Right val -> do
      let output = (fst . splitAt defaultMaxOutputLength) val
      if "*** Exception" `isInfixOf` output
        then (return . Left . UnknownError) val
        else (return . Right) output
    _ -> return result
  where
    quickCheckModules = ["Test.QuickCheck", "Test.QuickCheck.Gen", "Test.QuickCheck.Random"]

runChecks :: MonadIO m => Environment -> RType -> UProgram -> FilterTest m Bool
runChecks env goalType prog =
  and <$> mapM (\f -> f modules funcSig body) checks
  where
    (modules, funcSig, body, _) = extractSolution env goalType prog

    checks = [ checkSolutionNotCrash
             , checkDuplicates]

checkSolutionNotCrash :: MonadIO m => [String] -> String -> String -> FilterTest m Bool
checkSolutionNotCrash modules sigStr body = or <$> liftIO executeCheck
  where
    executeCheck = handleNotSupported $ do
      let funcSig = (instantiateSignature . parseTypeString) sigStr
      replicateM defaultNumChecks (check funcSig)

    check funcSig = do
      input <- generateTestInput modules funcSig
      let (inits, arg) = formatGenInputInitialization input
      let expression = fmtFunction_ body (show funcSig) arg

      let hos = filterHigherOrder_ funcSig
      result <- evalHOF_ modules inits expression defaultTimeoutMicro

      case result of
        Nothing -> putStrLn "Timeout in running always-fail detection" >> return False
        Just (Left err) -> putStrLn (displayException err) >> return False
        Just (Right res) -> return (seq res True)

handleNotSupported = (`catch` ((\ex -> print ex >> return [True]) :: NotSupportedException -> IO [Bool]))

-- some timeout happened to raise an exception with "timeout" keyword
-- turn it into Nothing rather than have it as exception
handleAnyException :: IO (Maybe (Either InterpreterError a)) -> IO (Maybe (Either InterpreterError a))
handleAnyException = (`catch` (return . handler . (show :: SomeException -> String)))
  where
    handler ex  | "timeout" `isInfixOf` ex = Nothing
                | otherwise = (Just . Left. UnknownError) ex
fmtFunction_ = printf "((%s) :: %s) %s" :: String -> String -> String -> String
fmtPureHO name typeStr seed size =
  printf "(%s) <- pure (((unGen arbitrary (mkQCGen (%d)) (%d)) :: (%s)))" name seed size typeStr :: String

-- returns (init statements for HOs, line of arguments value)
formatGenInputInitialization :: GeneratedInput -> ([String], String)
formatGenInputInitialization generated =
  (,) (map genStmtHO $ filter isHO generated) (unwords $ map show generated)
  where
    genStmtHO (HigherOrder tipe index seed size) =
      fmtPureHO (formatHigherOrderArgument index) tipe seed size
    
    isHO HigherOrder {} = True
    isHO _ = False

isHigherOrderArgument :: ArgumentType -> Bool
isHigherOrderArgument (ArgTypeFunc _ _) = True
isHigherOrderArgument (ArgTypeList sub) = isHigherOrderArgument sub
isHigherOrderArgument (ArgTypeApp l r) = any isHigherOrderArgument [l, r]
isHigherOrderArgument (ArgTypeTuple types) = any isHigherOrderArgument types
isHigherOrderArgument _ = False

filterHigherOrder_ :: FunctionSignature -> [String]
filterHigherOrder_ funcSig = map show $ filter isHigherOrderArgument $ _argsType funcSig

checkDuplicates :: MonadIO m => [String] -> String -> String -> FilterTest m Bool
checkDuplicates modules sigStr body = do
  st <- get
  case sampleResults st of
    Nothing -> do

      inputs <- liftIO $ generateInputs modules funcSig

      modify $ \st -> st { sampleResults = Just (SampleResult inputs []) }
      checkDuplicates modules sigStr body
    Just (SampleResult inputs results) -> do
      evals <- liftIO $ evalResults inputs body
      let isDuplicated = evals `elem` results
      let results' = if isDuplicated then results else evals:results

      modify $ \st -> st { sampleResults = Just $ SampleResult inputs results'}
      return $ not isDuplicated

      where
        evalResults inputs body = mapM evalWithArg inputs

        evalWithArg :: GeneratedInput -> IO SampleResultItem
        evalWithArg input = 
          let (inits, arg) = formatGenInputInitialization input in
          liftM2 (,)
            (pure $ formatArg arg)
            (show . formatEval <$> evalHOF_ modules inits (fmtFunction_ body (show funcSig) arg) defaultTimeoutMicro) 
          where
            formatArg = take defaultMaxArgShowLength

            formatEval Nothing = "timeout"
            formatEval (Just (Left error)) = "error: " ++ show error
            formatEval (Just (Right result)) = result
        

  where
    funcSig = (instantiateSignature . parseTypeString) sigStr
    hos = filterHigherOrder_ funcSig

    generateInputs :: [String] -> FunctionSignature -> IO [GeneratedInput]
    generateInputs modules funcSig = replicateM defaultNumChecks (generateTestInput modules funcSig)
