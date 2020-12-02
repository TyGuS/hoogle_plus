-- module HooglePlus.FilterTest (runChecks, checkSolutionNotCrash, checkDuplicates) where
{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.FilterTest where

import Language.Haskell.Interpreter hiding (get, set)
import qualified Language.Haskell.Interpreter as LHI
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

import Test.SmallCheck (Depth)
import Test.SmallCheck.Drivers

import HooglePlus.Utils
import Types.Environment
import Types.Program
import Types.Type hiding (typeOf)
import Types.Filtering
import Types.IOFormat (Example(Example))
import Synquid.Type
import Synquid.Program
import Paths_HooglePlus

import Debug.Trace

parseTypeString :: String -> FunctionSignature
parseTypeString input = FunctionSignature constraints argsType returnType
  where
    (constraints, argsType, returnType) = buildSig [] [] value
    (ParseOk value) = parseType input

    buildSig constraints argList (TyForall _ _ (Just ctx) t) = buildSig constraints' argList t
      where constraints' = constraints ++ extractConstraints constraints ctx
    buildSig constraints argList (TyFun _ typeArg typeRet) = buildSig constraints argList' typeRet
      where argList' = argList ++ [extractType typeArg]
    buildSig constraints argList (TyParen _ t) = buildSig constraints argList t
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

    extractQualified (TypeA _ t) = extractType t
    extractQualified (ParenA _ qual) = extractQualified qual
    extractQualified other = throw $ NotSupportedException ("Not able to extract " ++ show other)

    extractConstraints constraints (CxSingle _ item) = constraints ++ [extractQualified item]
    extractConstraints constraints (CxTuple _ list) = foldr ((++) . (:[]) . extractQualified) constraints list

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
      instantiate (ArgTypeFunc (Polymorphic name) r) = ArgTypeFunc (Concrete "MyInt") (instantiate r)
      instantiate (ArgTypeFunc l r) = ArgTypeFunc (instantiate l) (instantiate r)

buildFunctionWrapper :: [(String, String)] -> String -> (String, String, String, String) -> Int -> String
buildFunctionWrapper functions solutionType params@(plain, typed, shows, unwrp) timeInMicro =
    unwords
      (map (`buildLetFunction` solutionType) functions ++ [buildTimeoutWrapper (map fst functions) params timeInMicro])
  where
    buildLetFunction :: (String, String) -> String -> String
    buildLetFunction (wrapperName, solution) solutionType =
      printf "let %s = ((%s) :: %s) in" wrapperName solution (replaceId "MyInt" "Int" solutionType) :: String

    buildTimeoutWrapper :: [String] -> (String, String, String, String) -> Int -> String
    buildTimeoutWrapper wrapperNames (plain, typed, shows, unwrp) timeInMicro =
      printf "let timeoutWrapper = \\%s -> (evaluateIO %d %s (Prelude.map (\\internal__f -> internal__f %s) [%s])) in" typed timeInMicro shows unwrp (intercalate ", " wrapperNames) :: String

buildNotCrashProp :: [String] -> String -> FunctionSignature -> String
buildNotCrashProp argNames solution funcSig = formatAlwaysFailProp params wrapper
  where
    params@(plain, typed, shows, unwrp) = showParams argNames (_argsType funcSig)

    wrapper = buildFunctionWrapper [("wrappedSolution", solution)] (show funcSig) params defaultTimeoutMicro

    formatAlwaysFailProp = formatProp "propAlwaysFail" "isFailedResult"
    -- formatNeverFailProp = formatProp "propNeverFail" "not <$> isFailedResult"

    formatProp propName body (plain, typed, shows, unwrp) wrappedSolution = unwords
      [ wrappedSolution
      , printf "let %s %s = monadic (%s <$> Prelude.head <$> timeoutWrapper %s) in" propName plain body plain
      , printf "runStateT (smallCheckM %d (%s)) []" defaultDepth propName] :: String

buildDupCheckProp :: [String] -> (String, [String]) -> FunctionSignature -> Int -> Depth -> [String]
buildDupCheckProp argNames (sol, otherSols) funcSig timeInMicro depth =
  map (\x -> buildDupCheckProp' argNames (sol, [x]) funcSig timeInMicro depth) otherSols

buildDupCheckProp' :: [String] -> (String, [String]) -> FunctionSignature -> Int -> Depth -> String
buildDupCheckProp' argNames (sol, otherSols) funcSig timeInMicro depth =

  unwords [wrapper, formatProp]
  where
    params@(plain, typed, shows, unwrp) = showParams argNames (_argsType funcSig)
    solutionType = show funcSig

    wrapper = buildFunctionWrapper solutions (show funcSig) params defaultTimeoutMicro
    solutions = zip [printf "result_%d" x :: String | x <- [0..] :: [Int]] (sol:otherSols)

    formatProp = unwords
      [ printf "let dupProp = existsUnique $ \\%s -> monadic (not <$> anyDuplicate <$> timeoutWrapper %s) in" plain plain
      , printf "runStateT (smallCheckM %d dupProp) []" depth] :: String

runInterpreter' :: Int -> InterpreterT IO a -> IO (Either InterpreterError a)
runInterpreter' timeInMicro exec =
    toResult <$> timeout timeInMicro execute
  where
    execute = do
      srcPath <- getDataFileName "InternalTypeGen.hs"

      runInterpreter $ do

        -- allow extensions for function execution
        extensions <- LHI.get languageExtensions
        LHI.set [languageExtensions := (ExtendedDefaultRules : ScopedTypeVariables : extensions)]

        loadModules [srcPath]
        setTopLevelModules ["InternalTypeGen"]

        exec

    toResult Nothing = Left $ UnknownError "timeout"
    toResult (Just v) = v

evaluateProperty :: [String] -> String -> IO (Either InterpreterError SmallCheckResult)
evaluateProperty modules property =
  runInterpreter' defaultInterpreterTimeoutMicro $ do
    setImportsQ (zip modules (repeat Nothing) ++ frameworkModules)
    interpret property (as :: IO SmallCheckResult) >>= liftIO

validateSolution :: [String] -> [String] -> String -> FunctionSignature -> Int -> IO (Either InterpreterError FunctionCrashDesc)
validateSolution modules argNames solution funcSig time = evaluateResult' <$> evaluateProperty modules alwaysFailProp
  where
    alwaysFailProp = buildNotCrashProp argNames solution funcSig

    evaluateSmallCheckResult ::
      Either InterpreterError SmallCheckResult -> Either InterpreterError SmallCheckResult
        -> Either InterpreterError FunctionCrashDesc
    evaluateSmallCheckResult resultF resultS =
      case resultF of
        Left (UnknownError "timeout") -> Right $ AlwaysFail $ caseToInput resultS
        Right (Nothing, _) -> Right $ AlwaysFail $ caseToInput resultS
        Right (_, exF:_) -> case resultS of
          Left (UnknownError "timeout") -> Right $ AlwaysSucceed exF
          Right (Nothing, _) -> Right $ AlwaysSucceed exF

          Right (Just (CounterExample _ _), exS:_) -> Right $ PartialFunction [exF, exS]
          _ -> error (show resultF ++ "???" ++ show resultS)
        _ -> trace (show resultF) $ Right $ AlwaysFail $ caseToInput resultS

    evaluateResult' result = case result of
      Left (UnknownError "timeout") -> Right $ AlwaysFail $ Example [] "timeout"
      Left error -> trace (show error) (Right $ AlwaysFail $ Example [] (show error))
      Right (Nothing, _) -> Right $ AlwaysFail $ caseToInput result
      Right (_, examples) -> Right $ PartialFunction $ take 2 examples -- only need the first two examples, one succeed, one fail

    caseToInput :: Either InterpreterError SmallCheckResult -> Example
    caseToInput (Right (_, example:_)) = example

    preprocessOutput :: String -> String -> String
    preprocessOutput input output = trace ("ok: " ++ input ++ ", " ++ output) (fromMaybe "N/A" (listToMaybe selectedLine))
      where
        ios = nub $ filter ([] /=) $ lines output
        selectedLine = filter (isInfixOf input) ios

compareSolution :: [String] -> [String] -> String -> [String] -> FunctionSignature -> Int -> IO [Either InterpreterError SmallCheckResult]
compareSolution modules argNames solution otherSolutions funcSig time = mapM (evaluateProperty modules) props
  where props = buildDupCheckProp argNames (solution, otherSolutions) funcSig time defaultDepth

runChecks :: MonadIO m => Environment -> TypeSkeleton -> UProgram -> FilterTest m (Maybe AssociativeExamples)
runChecks env goalType prog = do
  result <- runChecks_

  state <- get
  when result $ liftIO $ runPrints state

  return $ if result then Just (collectExamples (unqualifyFunc body, body) state) 
                     else Nothing
  where
    (modules, funcSig, body, argList) = extractSolution env goalType prog
    argNames = map fst argList
    checks = [ checkSolutionNotCrash
             , checkDuplicates]

    runChecks_ = and <$> mapM (\f -> f modules argNames funcSig body) checks
    runPrints state = do
      putStrLn "\n*******************FILTER*********************"
      putStrLn (show $ unqualifyFunc body)
      putStrLn (printSolutionState (unqualifyFunc body, body) state)

checkSolutionNotCrash :: MonadIO m => [String] -> [String] -> String -> UProgram -> FilterTest m Bool
checkSolutionNotCrash modules argNames sigStr body = do
  fs@(FilterState _ _ examples _) <- get
  result <- liftIO executeCheck

  let pass = case result of
               Right (AlwaysFail _) -> False
               _ -> True

  let Right desc = result
  put $ fs {solutionExamples = ((unqualifyFunc body, body), desc) : examples}
  return pass

  where
    handleNotSupported = (`catch` ((\ex -> return (Left (UnknownError $ show ex))) :: NotSupportedException -> IO (Either InterpreterError FunctionCrashDesc)))
    funcSig = (instantiateSignature . parseTypeString) sigStr
    executeCheck = handleNotSupported $ validateSolution modules argNames (show body) funcSig defaultTimeoutMicro


checkDuplicates :: MonadIO m => [String] -> [String] -> String -> UProgram -> FilterTest m Bool
checkDuplicates modules argNames sigStr solution = do
  fs@(FilterState is solns _ examples) <- get
  case solns of

    -- no solution yet; skip the check
    [] -> do
      put fs {solutions = solution:solns}
      return True

    -- find an input i such that all synthesized results can be differentiated semantically
    _ -> do
      results <- liftIO $ compareSolution modules argNames (show solution) (map show solns) funcSig defaultTimeoutMicro
      passTest <- and <$> zipWithM processResult results solns

      fs'@(FilterState is solns _ examples) <- get
      if passTest
        then (put fs' {solutions = solution : solns})
        else (put fs)

      return passTest

  where
    funcSig = (instantiateSignature . parseTypeString) sigStr
    caseToInput (Just (AtLeastTwo i_1 _ i_2 _)) = [i_1, i_2]

    filterRelated i1 i2 (Example inputX _) = inputX == i1 || inputX == i2
    filterSuccess (Left (UnknownError "timeout")) = False
    filterSuccess (Left _) = True
    filterSuccess (Right (r@(Just AtLeastTwo {}), newExamples)) = True
    filterSuccess _ = False

    processResult result otherSolution = do
      state@(FilterState is solns _ examples) <- get
      case result of
        -- bypass the check on any timeout or error
        Left (UnknownError "timeout") -> return False -- no example -> reject
        Left err -> return True

        -- SmallCheck fails to find any differentiating input
        Right (Just NotExist, _) -> return False
        Right (Nothing, _) -> return False

        -- the trick to make SC to generate two inputs
        Right (r@(Just AtLeastTwo {}), newExamples) -> do
          let [i1, i2] = caseToInput r
          let sols = [ (unqualifyFunc solution, solution)
                     , (unqualifyFunc otherSolution, otherSolution)
                     ]
          put $ state {
            inputs = [i1, i2] ++ is,
            differentiateExamples = (zip sols (filter (filterRelated i1 i2) newExamples)) ++ examples
          }
          return True
        _ -> return False

-- show parameters to some Haskell representation
-- (plain variables, typed variables, list of show, unwrapped variables)
-- >> showParams ["Int"] => ("arg_0", "(arg_0 :: Inner Int)", "[show arg_0]", "(unwrap arg_0)")
showParams :: [String] -> [ArgumentType] -> (String, String, String, String)
showParams argNames args = (plain, typed, shows, unwrp)
  where
    args' = zip argNames $ map replaceInner args

    plain = unwords $ formatIdx "(%s)"
    unwrp = unwords $ formatIdx "(unwrap %s)"
    typed = unwords $ map (\(idx, tipe) -> printf "(%s :: %s)" idx (show tipe) :: String) args'

    shows = "[" ++ intercalate ", " (formatIdx "(show %s)") ++ "]"
    formatIdx format = map ((printf format :: String -> String) . fst) args'
    
    replaceInner :: ArgumentType -> ArgumentType
    replaceInner x =
      let apply a b = ArgTypeApp (ArgTypeApp (Concrete "MyFun") a) b in case x of
        Concrete _ -> x
        Polymorphic _ -> x
        ArgTypeList t -> ArgTypeList (replaceInner t)
        ArgTypeTuple ts -> ArgTypeTuple (map replaceInner ts)
        ArgTypeApp f a -> ArgTypeApp (replaceInner f) (replaceInner a)
        ArgTypeFunc arg res -> apply (replaceInner arg) (replaceInner res)

-- ******** Example Generator ********

generateIOPairs :: [String] -> String -> FunctionSignature -> [String] -> Int -> Int -> Int -> Depth -> [String] -> [[String]] -> IO (Either InterpreterError GeneratorResult)
generateIOPairs modules solution funcSig argNames numPairs timeInMicro interpreterTimeInMicro depth existingResults existingInputs =
  runInterpreter' interpreterTimeInMicro $ do
    setImportsQ (zip modules (repeat Nothing) ++ frameworkModules)
    interpret property (as :: IO GeneratorResult) >>= liftIO

  where
    funcSig' = instantiateSignature funcSig
    typeStr = show funcSig'
    params = showParams argNames (_argsType funcSig')
    property = buildProp solution funcSig'

    buildProp :: String -> FunctionSignature -> String
    buildProp solution funcSig =

      formatProp params wrapper
      where
        wrapper = buildWrapper solution typeStr params defaultTimeoutMicro

        formatProp (plain, typed, shows, unwrp) wrappedSolution = unwords
          [ wrappedSolution
          , printf "let previousResults = [%s] in" (intercalate ", " $ map show existingResults)
          , printf "let previousInputs = %s in" (show existingInputs)
          , printf "let prop %s = monadic ((wrappedSolution %s) >>= (waitState %d %s previousResults previousInputs)) in" plain plain numPairs shows
          , printf "execStateT (smallCheckM %d (exists prop)) []" depth] :: String

    buildWrapper solution typeStr params timeInMicro =
      unwords [ buildLetFunction solution typeStr
              , buildTimeoutWrapper params timeInMicro
              ]
      where
        buildLetFunction :: String -> String -> String
        buildLetFunction solution solutionType =
          printf "let sol_wrappedSolution = ((%s) :: %s) in" solution (replaceId "MyInt" "Int" typeStr) :: String

        buildTimeoutWrapper :: (String, String, String, String) -> Int -> String
        buildTimeoutWrapper (plain, typed, shows, unwrp) timeInMicro =
          printf "let wrappedSolution %s = (liftIO $ CB.timeOutMicro' %d (CB.approxShow %d (sol_wrappedSolution %s))) in" typed timeInMicro defaultMaxOutputLength unwrp :: String
