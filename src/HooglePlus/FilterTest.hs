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
import qualified Data.Map as Map hiding (map, foldr)
import qualified Data.Set as Set hiding (map)
import System.Timeout
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Typeable

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

buildFunctionWrapper :: String -> String -> String -> Int -> String
buildFunctionWrapper = buildFunctionWrapper' "wrappedSolution"

buildFunctionWrapper' :: String -> String -> String -> String -> Int -> String
buildFunctionWrapper' wrapperName solution solutionType solutionParams timeInMicro =
  unwords
  [ formatSolution solution solutionType
  , formatWrapper timeInMicro solutionParams]
  where

    formatSolution solution typeStr =
      printf "let sol_%s = ((%s) :: %s) in" wrapperName solution typeStr :: String
    formatWrapper timeInMicro params =
      printf "let %s %s = (CB.timeOutMicro' %d (CB.approxShow %d (sol_%s %s))) in" wrapperName params timeInMicro defaultMaxOutputLength wrapperName params :: String

buildNotCrashProp :: String -> FunctionSignature -> (String, String)
buildNotCrashProp solution funcSig =

  ( formatAlwaysFailProp argLine argDecl wrapper
  , formatNeverFailProp argLine argDecl wrapper)
  where
    (argLine, argDecl) = toParamListDecl (_argsType funcSig)

    wrapper = buildFunctionWrapper solution (show funcSig) argLine defaultTimeoutMicro

    formatAlwaysFailProp = formatProp "propAlwaysFail" "isFailedResult"
    formatNeverFailProp = formatProp "propNeverFail" "not <$> isFailedResult"

    formatProp propName body argLine argDecl wrappedSolution = unwords
      ([wrappedSolution] ++
      [ printf "let %s %s = monadic (%s <$> %s %s) in" propName argDecl body "wrappedSolution" argLine
      , printf "smallCheckM 10 (%s)" propName]) :: String

buildDupCheckProp :: (String, [String]) -> FunctionSignature -> Int -> String
buildDupCheckProp (sol, otherSols) funcSig timeInMicro =

  unwords [wrapperLhs, unwords wrapperSols, formatProp wrapperLhs wrapperSols argLine argDecl]
  where
    (argLine, argDecl) = toParamListDecl (_argsType funcSig)
    solutionType = show funcSig

    wrapFunc name sol = buildFunctionWrapper' name sol solutionType argLine timeInMicro

    otherSols' = zip [0..] otherSols
    wrapperLhs = wrapFunc "lhs" sol
    wrapperSols = map (\(i, sol) -> wrapFunc (printf "sol_%d" i) sol) otherSols'

    formatBinding :: (Int, String) -> String
    formatBinding (i, _) = printf "result_%d <- run (sol_%d %s);" i i argLine :: String

    formatBindingItem :: (Int, String) -> String
    formatBindingItem (i, _) = printf "result_%d" i :: String

    formatBindingList sols =
      let items = intercalate "," $ map formatBindingItem sols in
        printf "[%s]" items :: String

    formatProp wLhs wRhs argLine argDecl = unwords
      ([ printf "let dupProp %s = monadic $ do {" argDecl 
      , printf "resultL <- run (lhs %s);" argLine]
        ++ map formatBinding otherSols' ++
      [ printf "assert (Prelude.or $ Prelude.map (isEqualResult resultL) %s)" (formatBindingList otherSols')
      , "} in"
      , printf "quickCheckWithResult stdArgs { chatty = False } dupProp"]) :: String

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
  result <- runInterpreter' defaultInterpreterTimeoutMicro $ do
    setImportsQ (zip modules (repeat Nothing) ++ frameworkModules)

    let (alwaysFailProp, neverFailProp) = buildNotCrashProp solution funcSig

    alwaysFailResult <- interpret alwaysFailProp (as :: IO SmallCheckResult) >>= liftIO
    neverFailResult <- interpret neverFailProp (as :: IO SmallCheckResult) >>= liftIO

    return (alwaysFailResult, neverFailResult)
  case result of
    Left err -> return $ Left err
    Right (Nothing, failResult) -> return $ Right $ AlwaysFail $ caseToString failResult
    Right (succeedResult, Nothing) -> return $ Right $ AlwaysSucceed $ caseToString succeedResult
    Right (succeedResult, failResult) -> return $ Right (PartialFunction (caseToString succeedResult) (caseToString failResult))

  where
    caseToString (Just (CounterExample args _)) = unwords args

compareSolution :: [String] -> String -> [String] -> FunctionSignature -> Int -> IO (Either InterpreterError SmallCheckResult)
compareSolution modules solution otherSolutions funcSig time = do
  runInterpreter' defaultInterpreterTimeoutMicro $ do
    setImportsQ (zip modules (repeat Nothing) ++ frameworkModules)

    let prop = buildDupCheckProp (solution, otherSolutions) funcSig time
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
  result <- liftIO $ executeCheck

  let (pass, desc) = case result of
                          Left err -> (True, UnableToCheck (show err))
                          Right desc@(AlwaysFail _) -> (False, desc)
                          Right desc -> (True, desc)

  modify $ const fs {solutionExamples = (body, desc):samples};
  return pass

  where
    handleNotSupported = (`catch` ((\ex -> return (Left ((UnknownError $ show ex)))) :: NotSupportedException -> IO (Either InterpreterError FunctionCrashDesc)))
    executeCheck = handleNotSupported $ do
      let funcSig = (instantiateSignature . parseTypeString) sigStr
      validateSolution modules body funcSig defaultTimeoutMicro
      

checkDuplicates :: MonadIO m => [String] -> String -> String -> FilterTest m Bool
checkDuplicates modules sigStr solution = do
  fs@(FilterState is solns _) <- get

  result <- liftIO $ compareSolution modules solution solns funcSig defaultTimeoutMicro

  case result of
    Left err -> do
      liftIO $ print err
      modify $ const fs {solutions = solution:solns}
      return True
    Right (Just (CounterExample args _)) -> do
      modify $ const fs {inputs = ["todo"]:is, solutions = solution:solns}
      return True
    _ -> return False

  where
    funcSig = (instantiateSignature . parseTypeString) sigStr

toParamListDecl :: [ArgumentType] -> (String, String)
toParamListDecl args =

  (plainArgLine, declArgLine)

  where
    n = length args
    indexedArgs = zip [1..n] args

    plainArgLine = unwords $ map (formatParam . fst) indexedArgs
    declArgLine = unwords $ map toDecl indexedArgs
    
    computeAppDepth (ArgTypeFunc l r) =
      case r of
        ArgTypeFunc _ _ -> 1 + computeAppDepth r
        _ -> 1
    computeAppDepth invalid = error $ printf "Invalid argument for computeAppDepth: %s" (show invalid)

    formatParam = printf "arg_%d" :: Int -> String

    toDecl :: (Int, ArgumentType) -> String
    toDecl (index, tipe@(ArgTypeFunc l r)) =

      printf "(%s arg_%d)" pattern index
      where
        pattern = case computeAppDepth tipe of
          1 -> "Fn"
          2 -> "Fn2"
          3 -> "Fn3"
          _ -> error "Unsupported higher-order function"
    
    -- todo: previously we use _Universal_ to restrict range
    -- but now we don't necessarily need it. Consider remove
    toDecl (index, _) = printf "(arg_%d)" index
      
      