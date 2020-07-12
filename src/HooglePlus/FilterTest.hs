{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns #-}
module HooglePlus.FilterTest where

import qualified Language.Haskell.Interpreter as LHI
import qualified Test.QuickCheck as QC
import qualified Data.Map as Map

import Language.Haskell.Interpreter (InterpreterT, InterpreterError(..), Extension(..), OptionVal(..))

import Language.Haskell.Exts.Parser
import Text.Printf
import Control.Exception
import Data.List
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import System.Timeout
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Typeable
import Data.Either

import HooglePlus.Utils (splitConsecutive, printSolutionState, collectExamples, extractSolution)
import Types.Environment
import Types.Program
import Types.Type hiding (typeOf)
import Types.Filtering
import Types.IOFormat (Example(Example))
import Synquid.Type
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
      instantiate (ArgTypeFunc l r) = ArgTypeFunc (instantiate l) (instantiate r)

buildFunctionWrapper :: [(String, String)] -> String -> (String, String, String, String) -> Int -> String
buildFunctionWrapper functions solutionType params@(plain, typed, shows, unwrp) timeInMicro =
    unwords
      (map (`buildLetFunction` solutionType) functions ++ [buildTimeoutWrapper (map fst functions) params timeInMicro])
  where
    buildLetFunction :: (String, String) -> String -> String
    buildLetFunction (wrapperName, solution) solutionType =
      printf "let %s = ((%s) :: %s) in" wrapperName solution solutionType :: String

    buildTimeoutWrapper :: [String] -> (String, String, String, String) -> Int -> String
    buildTimeoutWrapper wrapperNames (plain, typed, shows, unwrp) timeInMicro =
      printf "let timeoutValue = \\%s -> (evaluateValue %d (Prelude.map (\\f -> f %s) [%s])) in" typed timeInMicro unwrp (intercalate ", " wrapperNames) :: String

buildNotCrashProp :: String -> FunctionSignature -> String
buildNotCrashProp solution funcSig = formatNotCrashProp params wrapper
  where
    params@(plain, typed, shows, unwrp) = showParams (_argsType funcSig)

    wrapper = buildFunctionWrapper [("wrappedSolution", solution)] (show funcSig) params defaultTimeoutMicro

    formatNotCrashProp  = formatProp "prop_not_crash" "not $ isFailedResult $ Prelude.head out"
    -- formatCrashProp     = formatProp "prop_crash" "isFailedResult $ Prelude.head out"

    formatProp propName propBody (plain, typed, shows, unwrp) wrappedSolution = unwords
      [ wrappedSolution
      , printf "let %s %s = monadicIO $ run $ do { out <- timeoutWrapper %s; return $ labelProperty %s out ((%s) ==> True); } in" propName plain plain shows propBody
      , printf "quickCheckResult %s" propName ] :: String

buildDupCheckProp :: (String, [String]) -> FunctionSignature -> Int -> [String]
buildDupCheckProp (sol, otherSols) funcSig timeInMicro =
  map (\x -> buildDupCheckProp' (sol, [x]) funcSig timeInMicro) otherSols

buildDupCheckProp' :: (String, [String]) -> FunctionSignature -> Int -> String
buildDupCheckProp' (sol, otherSols) funcSig timeInMicro =

  unwords [wrapper, formatProp]
  where
    params@(plain, typed, shows, unwrp) = showParams (_argsType funcSig)
    solutionType = show funcSig

    wrapper = buildFunctionWrapper solutions (show funcSig) params defaultTimeoutMicro
    solutions = zip [printf "result_%d" x :: String | x <- [0..] :: [Int]] (sol:otherSols)

    formatProp = unwords
      [ printf "let prop_duplicate %s = monadicIO $ run $ do { out <- timeoutWrapper %s; return $ labelProperty %s out ((anyDuplicate out) ==> True); } in" plain plain shows
      , printf "quickCheckResult prop_duplicate" ] :: String

runInterpreter' :: Int -> InterpreterT IO a -> IO (Either InterpreterError a)
runInterpreter' timeInMicro exec = toResult <$> timeout timeInMicro execute
  where
    execute = do
      srcPath <- getDataFileName "InternalTypeGen.hs"
      LHI.runInterpreter $ do
        extensions <- LHI.get LHI.languageExtensions
        LHI.set [LHI.languageExtensions := (ExtendedDefaultRules : ScopedTypeVariables : extensions)]

        LHI.loadModules [srcPath]
        LHI.setTopLevelModules ["InternalTypeGen"]

        exec

    toResult = \case
      Nothing -> Left $ NotAllowed "timeout"
      Just v  -> v

evaluateProperty :: [String] -> String -> IO (Either InterpreterError BackendResult)
evaluateProperty modules property =
  runInterpreter' defaultInterpreterTimeoutMicro $ do
    LHI.setImportsQ (zip modules (repeat Nothing) ++ frameworkModules)
    LHI.interpret property (LHI.as :: IO BackendResult) >>= liftIO

validateSolution :: [String] -> String -> FunctionSignature -> Int -> IO (Either InterpreterError FuncTestDesc)
validateSolution modules solution funcSig time = do
    result <- evaluateProperty modules prop
    case result of
      Left  (NotAllowed _)            -> return $ Right $ Unknown "timeout"
      Left  error                     -> return $ Right $ Unknown $ show error
      Right result                    -> return $ Right $ readResult result
  where
    prop = buildNotCrashProp solution funcSig

    readResult :: BackendResult -> FuncTestDesc
    readResult r@QC.GaveUp{QC.numTests, QC.numDiscarded}
              | numTests == numDiscarded  = Invalid
              | otherwise                 = Partial $ parseExamples r
    readResult r@QC.Success{} = Total $ parseExamples r

compareSolution :: [String] -> String -> [String] -> FunctionSignature -> Int -> IO [Either InterpreterError BackendResult]
compareSolution modules solution otherSolutions funcSig time = mapM (evaluateProperty modules) props
  where props = buildDupCheckProp (solution, otherSolutions) funcSig time

runChecks :: MonadIO m => Environment -> TypeSkeleton -> UProgram -> FilterTest m (Maybe AssociativeExamples)
runChecks env goalType prog = do
  result <- runChecks_

  state <- get
  when result $ liftIO $ runPrints state

  return $ if result then Just (collectExamples body state) else Nothing
  where
    (modules, funcSigStr, body, _) = extractSolution env goalType prog
    checks = [ checkSolutionNotCrash
             , checkDuplicates]

    funcSig = (instantiateSignature . parseTypeString) funcSigStr

    runChecks_ = and <$> mapM (\f -> f modules funcSig body) checks
    runPrints state = do
      putStrLn "\n*******************FILTER*********************"
      putStrLn body
      putStrLn (printSolutionState body state)

checkSolutionNotCrash :: MonadIO m => [String] -> FunctionSignature -> String -> FilterTest m Bool
checkSolutionNotCrash modules funcSig solution = do
  result <- liftIO executeCheck
  case result of
    Left  _       -> return True
    Right result  -> do
      modify $ \s -> s {solutionDescriptions = (solution, result) : solutionDescriptions s }
      return $ isSuccess result

  where
    handleNotSupported = (`catch` ((\ex -> return (Left (UnknownError $ show ex))) :: NotSupportedException -> IO (Either InterpreterError FuncTestDesc)))
    executeCheck = handleNotSupported $ validateSolution modules solution funcSig defaultTimeoutMicro


checkDuplicates :: MonadIO m => [String] -> FunctionSignature -> String -> FilterTest m Bool
checkDuplicates modules funcSig solution = do
  FilterState _ solns_ _ _ <- get
  case solns_ of
    -- base case: skip the check for the first solution
    [] -> (modify $ \s -> s {solutions = [solution]}) >> return True

    -- inductive: find an input i such that all synthesized results can be differentiated semantically
    _ -> do
      results       <-  liftIO $ compareSolution modules solution solns_ funcSig defaultTimeoutMicro
      examples      <-  zipWithM readInterpreterResult solns_ results

      let isSucceed =   all isJust examples
      if  not isSucceed then return False else do
        let examples' = Map.unionsWith (++) $ map (Map.fromList . fromJust) examples
        modify $ \s -> s {
          solutions             = solution : solutions s,
          differentiateExamples = Map.unionWith (++) examples' (differentiateExamples s)
        }

        return True
  where
    readResult :: String -> BackendResult -> Maybe AssociativeExamples
    readResult prevSolution r@QC.GaveUp {QC.numTests, QC.numDiscarded}
              | numTests == numDiscarded  = Nothing
              | otherwise                 = Just [(solution, examples), (prevSolution, examplesForPrev)]
      where
        (examples, examplesForPrev) = splitConsecutive $ parseExamples r

    readInterpreterResult :: MonadIO m => String -> Either InterpreterError BackendResult -> FilterTest m (Maybe AssociativeExamples)
    readInterpreterResult prevSolution =
      \case
        Left (NotAllowed _) -> return Nothing
        Left _              -> return $ Just []
        Right result        -> return $ readResult prevSolution result

-- show parameters to some Haskell representation
-- (plain variables, typed variables, list of show, unwrapped variables)
-- >> showParams ["Int"] => ("arg_0", "(arg_0 :: Inner Int)", "[show arg_0]", "(unwrap arg_0)")
showParams :: [ArgumentType] -> (String, String, String, String)
showParams args = (plain, typed, shows, unwrp)
  where
    args' = zip [1..] $ map replaceInner args

    plain = unwords $ formatIdx "(arg_%d)"
    unwrp = unwords $ formatIdx "(unwrap arg_%d)"
    typed = unwords $ map (\(idx, tipe) -> printf "(arg_%d :: %s)" idx (show tipe) :: String) args'

    shows = "[" ++ intercalate ", " (formatIdx "(show arg_%d)") ++ "]"
    formatIdx format = map ((printf format :: Int -> String) . fst) args'

    replaceInner :: ArgumentType -> ArgumentType
    replaceInner x =
      let apply a b = ArgTypeApp (ArgTypeApp (Concrete "MyFun") a) b in case x of
        Concrete _ -> x
        Polymorphic _ -> x
        ArgTypeList t -> ArgTypeList (replaceInner t)
        ArgTypeTuple ts -> ArgTypeTuple (map replaceInner ts)
        ArgTypeApp f a -> ArgTypeApp (replaceInner f) (replaceInner a)
        ArgTypeFunc arg res -> apply (replaceInner arg) (replaceInner res)

-- parse Example string from QC.label to Example
parseExamples :: BackendResult -> [Example]
parseExamples result = map (head . read . head) $ Map.keys $ QC.labels result

-- ******** Example Generator ********

generateIOPairs :: [String] -> String -> FunctionSignature -> Int -> Int -> Int -> [String] -> [[String]] -> IO (Either InterpreterError GeneratorResult)
generateIOPairs modules solution funcSig numPairs timeInMicro interpreterTimeInMicro existingResults existingInputs = error "not implemented"
