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
    unwords [ buildLetFunction wrapperName solution solutionType
            , buildTimeoutWrapper wrapperName solutionParams timeInMicro
            ]
  where
    buildLetFunction :: String -> String -> String -> String
    buildLetFunction wrapperName solution solutionType = 
      printf "let sol_%s = ((%s) :: %s) in" wrapperName solution solutionType :: String

    buildTimeoutWrapper :: String -> String -> Int -> String
    buildTimeoutWrapper wrapperName params timeInMicro =
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

  return (evaluateSmallCheckResult resultF resultS)

  where
    (alwaysFailProp, neverFailProp) = buildNotCrashProp solution funcSig
    run prop = runInterpreter' defaultInterpreterTimeoutMicro $ do
      setImportsQ (zip modules (repeat Nothing) ++ frameworkModules)
      interpret prop (as :: IO SmallCheckResult) >>= liftIO

    evaluateSmallCheckResult ::
      Either InterpreterError SmallCheckResult -> Either InterpreterError SmallCheckResult
        -> Either InterpreterError FunctionCrashDesc
    evaluateSmallCheckResult resultF resultS =
      case resultF of
        Left (UnknownError "timeout") -> Right $ AlwaysFail $ caseToInput resultS
        Right Nothing -> Right $ AlwaysFail $ caseToInput resultS
        _ -> case resultS of
          Left (UnknownError "timeout") -> Right $ AlwaysSucceed $ caseToInput resultF
          Right Nothing -> Right $ AlwaysSucceed $ caseToInput resultF

          Right (Just (CounterExample _ _)) -> Right $ PartialFunction (caseToInput resultF) (caseToInput resultS)

    caseToInput :: Either InterpreterError SmallCheckResult -> SampleInput
    caseToInput (Right (Just (CounterExample args _))) = args
    caseToInput _ = []

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
                          Right (AlwaysFail example) -> (False, [])
                          Right (AlwaysSucceed example) -> (True, example)
                          Right (PartialFunction example _) -> (True, example)

  if not $ null desc
    then do {
      result <- liftIO $ getOutput modules desc body funcSig;
      case result of
        Right io -> do
          modify $ const fs {solutionExamples = (body, io):samples};
          return pass
        _ -> return pass
    }
    else
      return pass
                          

  where
    handleNotSupported = (`catch` ((\ex -> return (Left (UnknownError $ show ex))) :: NotSupportedException -> IO (Either InterpreterError FunctionCrashDesc)))
    funcSig = (instantiateSignature . parseTypeString) sigStr
    executeCheck = handleNotSupported $ validateSolution modules body funcSig defaultTimeoutMicro
      

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
      let [i1, i2] = caseToInput r
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

    caseToInput (Just (AtLeastTwo i_1 _ i_2 _)) = [i_1, i_2]

    getSucceedRes params s = do
        es <- mapM (\p -> getOutput modules p s funcSig) params
        return $ zip (repeat s) (filter (not . null) (rights es))

    overallSolutions params = mapM (getSucceedRes params)

-- Function Signature -> (Parameter Declaration, Arguments)
-- they may differ for higher-order functions
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
      
      
getOutput :: [String] -> SampleInput -> String -> FunctionSignature -> IO (Either InterpreterError IOExample)
getOutput modules sampleInput solution funcSig =
    runInterpreter' defaultInterpreterTimeoutMicro $ do
      let (argLine, _) = toParamListDecl (_argsType funcSig)
      let sampleInput' = unwords sampleInput
      let solType = show funcSig
      let solWrapper = buildFunctionWrapper "wrappedSolution" solution solType argLine defaultTimeoutMicro 
      let template = unwords ["%s catch",
                              "(evaluate (show $ sol_wrappedSolution %s))",
                              "(\\(e :: SomeException) -> return (show e))"]
      let prog = printf template solWrapper sampleInput'
      set [languageExtensions := [ScopedTypeVariables]]
      setImportsQ (zip ("Control.Exception":modules) (repeat Nothing))
      
      val <- interpret prog (as :: IO String) >>= liftIO
      return (sampleInput, val)
