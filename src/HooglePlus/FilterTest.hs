-- module HooglePlus.FilterTest (runChecks, checkSolutionNotCrash, checkDuplicates) where
module HooglePlus.FilterTest where

import Language.Haskell.Interpreter hiding (get)
import Language.Haskell.Interpreter.Unsafe
import Language.Haskell.Exts.Parser
import Text.Printf
import Control.Exception
import Data.List
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import qualified Data.Map as Map
import qualified Data.Set as Set hiding (map)
import System.Timeout
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Typeable
import Data.Map (Map)
import Data.Either

import Test.SmallCheck.Drivers

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

buildFunctionWrapper :: String -> String -> String -> String -> Int -> String
buildFunctionWrapper wrapperName solution solutionType solutionParams timeInMicro = 
    unwords [ buildFunctionWrapper' wrapperName solution solutionType
            , buildCBWrapper wrapperName solutionParams timeInMicro
            ]

buildFunctionWrapper' :: String -> String -> String -> String
buildFunctionWrapper' wrapperName solution solutionType =
    formatSolution solution solutionType
  where
    formatSolution solution typeStr =
      printf "let sol_%s = ((%s) :: %s) in" wrapperName solution typeStr :: String

buildCBWrapper :: String -> String -> Int -> String
buildCBWrapper wrapperName solutionParams timeInMicro =
  formatWrapper timeInMicro solutionParams
  where
    formatWrapper timeInMicro params =
      printf "let %s %s = (CB.timeOutMicro' %d (CB.approxShow %d (sol_%s %s))) in" wrapperName params timeInMicro defaultMaxOutputLength wrapperName params :: String

buildNotCrashProp :: String -> FunctionSignature -> (String, String)
buildNotCrashProp solution funcSig =

  ( formatAlwaysFailProp argLine argDecl wrapper
  , formatNeverFailProp argLine argDecl wrapper)
  where
    (argLine, argDecl) = toParamListDecl (_argsType funcSig)

    wrapper = buildFunctionWrapper "wrappedSolution" solution (show funcSig) argLine defaultTimeoutMicro

    formatAlwaysFailProp = formatProp "propAlwaysFail" "isFailedResult"
    formatNeverFailProp = formatProp "propNeverFail" "not <$> isFailedResult"

    formatProp propName body argLine argDecl wrappedSolution = unwords
      ([wrappedSolution] ++
      [ printf "let %s %s = monadic (%s <$> %s %s) in" propName argDecl body "wrappedSolution" argLine
      , printf "smallCheckM 10 (%s)" propName]) :: String

buildDupCheckProp :: (String, [String]) -> FunctionSignature -> Int -> Int -> String
buildDupCheckProp (sol, otherSols) funcSig timeInMicro depth =

  unwords [wrapperLhs, unwords wrapperSols, formatProp]
  where
    (argLine, argDecl) = toParamListDecl (_argsType funcSig)
    solutionType = show funcSig

    otherSols' = zip [0..] otherSols :: [(Int, String)]
    wrapperLhs = wrapFunc "lhs" sol
    wrapperSols = map (\(i, sol) -> wrapFunc (printf "result_%d" i) sol) otherSols'

    formatProp = unwords
      [ printf "let dupProp = existsUnique $ \\%s -> monadic $ do {" argDecl
      , printf "evaluated <- mapM (\\f -> f %s) (%s);" argLine (formatResultList otherSols')
      , printf "resultL <- lhs %s;" argLine
      , printf "return $ not (resultL `Prelude.elem` evaluated)"
      , "} in"
      , printf "smallCheckM %d dupProp" depth] :: String

    wrapFunc name sol = buildFunctionWrapper name sol solutionType argLine timeInMicro 
    formatResultList results =
      let items = intercalate "," $ map format results in
        printf "[%s]" items :: String
      where format (i, _) = printf "result_%d" i :: String

runInterpreter' :: Int -> InterpreterT IO a -> IO (Either InterpreterError a) 
runInterpreter' timeInMicro exec =
    toResult <$> timeout timeInMicro execute
  where
    execute = do
      srcPath <- getDataFileName "InternalTypeGen.hs"
      unsafeRunInterpreterWithArgs ["-fno-omit-yields"] $ do
    
        loadModules [srcPath]
        setTopLevelModules ["InternalTypeGen"]
    
        exec

    toResult Nothing = Left $ UnknownError "timeout"
    toResult (Just v) = v 


validateSolution :: [String] -> String -> FunctionSignature -> Int -> IO (Either InterpreterError FunctionCrashDesc)
validateSolution modules solution funcSig time = do
  resultF <- run alwaysFailProp
  resultS <- run neverFailProp

  case resultF of
    Left (UnknownError "timeout") -> return $ Right $ AlwaysFail []
    Right Nothing -> return $ Right $ AlwaysFail []
    _ -> do
        let params = caseToString resultF
        out <- evalResult params
        case resultS of
          Left (UnknownError "timeout") -> return $ Right $ AlwaysSucceed out
          Right Nothing -> return $ Right $ AlwaysSucceed out
          Right (Just (CounterExample _ _)) -> return $ Right $ PartialFunction out (caseToString resultS)
          _ -> return $ Left $ UnknownError "nonexhaustive pattern"

  where
    (alwaysFailProp, neverFailProp) = buildNotCrashProp solution funcSig
    run prop = runInterpreter' defaultInterpreterTimeoutMicro $ do
      setImportsQ (zip modules (repeat Nothing) ++ frameworkModules)
      interpret prop (as :: IO SmallCheckResult) >>= liftIO
    evalResult params = do
        res <- getOutput modules (unwords params) solution funcSig
        case res of
          Left err -> error $ show err
          Right r -> return r
    caseToString (Right (Just (CounterExample args _))) = args
    caseToString _ = ["N/A"]

compareSolution :: [String] -> String -> [String] -> FunctionSignature -> Int -> IO (Either InterpreterError SmallCheckResult)
compareSolution modules solution otherSolutions funcSig time =
  runInterpreter' defaultInterpreterTimeoutMicro $ do
    setImportsQ (zip modules (repeat Nothing) ++ frameworkModules)

    let prop = buildDupCheckProp (solution, otherSolutions) funcSig time defaultDepth
    interpret prop (as :: IO SmallCheckResult) >>= liftIO

runChecks :: MonadIO m => Environment -> RType -> UProgram -> FilterTest m Bool
runChecks env goalType prog =
  and <$> mapM (\f -> f modules funcSig body) checks
  where
    (modules, funcSig, body, _) = extractSolution env goalType prog

    checks = [ checkSolutionNotCrash
             , checkDuplicates]

checkSolutionNotCrash :: MonadIO m => [String] -> String -> String -> FilterTest m Bool
checkSolutionNotCrash modules sigStr body = do
  fs@(FilterState _ _ samples) <- get
  result <- liftIO executeCheck

  let (pass, desc) = case result of
                          Left err -> (True, [])
                          Right (AlwaysFail examples) -> (False, [])
                          Right (AlwaysSucceed examples) -> (True, examples)
                          Right (PartialFunction examples _) -> (True, examples)

  modify $ const fs {solutionExamples = if null desc then samples else (body, desc):samples};
  return pass

  where
    handleNotSupported = (`catch` ((\ex -> return (Left (UnknownError $ show ex))) :: NotSupportedException -> IO (Either InterpreterError FunctionCrashDesc)))
    executeCheck = handleNotSupported $ do
      let funcSig = (instantiateSignature . parseTypeString) sigStr
      validateSolution modules body funcSig defaultTimeoutMicro
      

checkDuplicates :: MonadIO m => [String] -> String -> String -> FilterTest m Bool
checkDuplicates modules sigStr solution = do
  fs@(FilterState is solns samples) <- get

  result <- liftIO $ compareSolution modules solution solns funcSig defaultTimeoutMicro
  case result of
    Left (UnknownError "timeout") -> return False
    Left err -> do
      modify $ const fs {solutions = solution:solns}
      return True

    Right r@(Just AtLeastTwo {}) -> do
      let [i1, i2] = caseToString r
      samplesNew <- liftIO $ overallSolutions [i1, i2] (solution:solns)
      modify $ const fs {
        inputs = if null solns then is else (i1, i2):is,
        solutions = solution : solns,
        solutionExamples = concat samplesNew ++ samples
      }
      return True

    Right (Just NotExist) -> return False
    Right Nothing -> return False
    _ -> return False

  where
    funcSig = (instantiateSignature . parseTypeString) sigStr

    caseToString (Just (AtLeastTwo i_1 _ i_2 _)) = [unwords i_1, unwords i_2]

    getSucceedRes params s = do
        es <- mapM (\p -> getOutput modules p s funcSig) params
        return $ zip (repeat s) (filter (not . null) (rights es))

    overallSolutions params = mapM (getSucceedRes params)

toParamListDecl :: [ArgumentType] -> (String, String)
toParamListDecl args =

  (plainArgLine, declArgLine)

  where
    n = length args
    indexedArgs = zip [1..n] args

    plainArgLine = unwords $ map (formatParam . fst) indexedArgs
    declArgLine = unwords $ map toDecl indexedArgs

    formatParam = printf "arg_%d" :: Int -> String

    toDecl :: (Int, ArgumentType) -> String
    toDecl (index, _) = printf "(arg_%d)" index
      
      
getOutput :: [String] -> String -> String -> FunctionSignature -> IO (Either InterpreterError [String])
getOutput modules params solution funcSig = do
    runInterpreter' defaultInterpreterTimeoutMicro $ do
        let solType = show funcSig
        let solWrapper = buildFunctionWrapper' "wrappedSolution" solution solType
        let template = unwords ["%s catch ((++) (words \"%s\") . (: []) <$>",
                                "evaluate (show $ sol_wrappedSolution %s))",
                                "(\\(e :: SomeException) -> return [])"]
        let prog = printf template solWrapper params params
        set [languageExtensions := [ScopedTypeVariables]]
        setImportsQ (zip ("Control.Exception":modules) (repeat Nothing))
        interpret prog (as :: IO [String]) >>= liftIO
