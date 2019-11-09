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

buildFunctionWrapper :: FunctionSignature -> String -> Int -> String
buildFunctionWrapper = buildFunctionWrapper' "wrappedSolution"

buildFunctionWrapper' :: String -> FunctionSignature -> String -> Int -> String
buildFunctionWrapper' wrapperName funcSig solution timeInMicro =
  unwords
  [ formatSolution solution (show funcSig)
  , formatWrapper timeInMicro argLine]
  where
    argLine = toParamListDecl (length (_argsType funcSig))

    formatSolution solution typeStr =
      printf "let sol_%s = ((%s) :: %s) in" wrapperName solution typeStr :: String
    formatWrapper timeInMicro argLine =
      printf "let %s %s = (CB.timeOutMicro' %d (CB.approxShow %d (sol_%s %s))) in" wrapperName argLine timeInMicro defaultMaxOutputLength wrapperName argLine :: String

buildNotCrashProp :: FunctionSignature -> (String, String)
buildNotCrashProp funcSig =

  (formatAlwaysFailProp argLine, formatNeverFailProp argLine)
  where
    argLine = toParamListDecl (length (_argsType funcSig))

    formatAlwaysFailProp = formatProp "propAlwaysFail" "isFailedResult result"
    formatNeverFailProp = formatProp "propNeverFail" "not $ isFailedResult result"

    formatProp propName body argLine = unwords
      [ printf "let %s %s = monadicIO $ do {" propName argLine
      , printf "result <- run (%s %s);" "wrappedSolution" argLine
      , printf "assert (%s)" body
      , "} in"
      , printf "quickCheckResult (%s)" propName] :: String

buildDupCheckProp :: (String, [String]) -> FunctionSignature -> Int -> String
buildDupCheckProp (sol, otherSols) funcSig timeInMicro =

  unwords [wrapperLhs, unwords wrapperSols, formatProp wrapperLhs wrapperSols argLine]
  where
    argLine = toParamListDecl (length (_argsType funcSig))

    otherSols' = zip [0..] otherSols
    wrapperLhs = buildFunctionWrapper' "lhs" funcSig sol timeInMicro
    wrapperSols = map (\(i, sol) -> buildFunctionWrapper' (printf "sol_%d" i) funcSig sol timeInMicro) otherSols'

    formatBinding :: (Int, String) -> String
    formatBinding (i, _) = printf "result_%d <- run (sol_%d %s);" i i argLine :: String

    formatBindingItem :: (Int, String) -> String
    formatBindingItem (i, _) = printf "result_%d" i :: String

    formatBindingList sols =
      let items = intercalate "," $ map formatBindingItem sols in
        printf "[%s]" items :: String

    formatProp wLhs wRhs argLine = unwords
      ([ printf "let dupProp %s = monadicIO $ do {" argLine
      , printf "resultL <- run (lhs %s);" argLine]
        ++ (map formatBinding otherSols') ++
      [ printf "assert (Prelude.or $ Prelude.map (isEqualResult resultL) %s)" (formatBindingList otherSols')
      , "} in"
      , printf "quickCheckResult dupProp"]) :: String

validateSolution :: [String] -> String -> FunctionSignature -> Int -> IO (Either InterpreterError FunctionCrashKind)
validateSolution modules solution funcSig time = do
  srcPath <- getDataFileName "InternalTypeGen.hs"
  result <- unsafeRunInterpreterWithArgs ["-fno-omit-yields"] $ do

    loadModules [srcPath]
    setTopLevelModules ["InternalTypeGen"]
    setImportsQ (zip modules (repeat Nothing) ++ quickCheckModules)

    let wrapper = buildFunctionWrapper funcSig solution time
    let (alwaysFailProp, neverFailProp) = buildNotCrashProp funcSig

    let alwaysFailProp' = wrapper ++ " " ++ alwaysFailProp
    let neverFailProp' = wrapper ++ " " ++ neverFailProp

    alwaysFailResult <- interpret alwaysFailProp' (as :: IO Result) >>= liftIO
    neverFailResult <- interpret neverFailProp' (as :: IO Result) >>= liftIO

    return (alwaysFailResult, neverFailResult)
  case result of
    Left err -> return $ Left err
    Right (Success{}, _) -> return $ Right AlwaysFail
    Right (_, Success{}) -> return $ Right AlwaysSucceed
    Right _ -> return $ Right PartialFunction

compareSolution :: [String] -> String -> [String] -> FunctionSignature -> Int -> IO (Either InterpreterError Result)
compareSolution modules solution otherSolutions funcSig time = do
  srcPath <- getDataFileName "InternalTypeGen.hs"
  unsafeRunInterpreterWithArgs ["-fno-omit-yields"] $ do

    loadModules [srcPath]
    setTopLevelModules ["InternalTypeGen"]
    setImportsQ (zip modules (repeat Nothing) ++ quickCheckModules)

    let prop = buildDupCheckProp (solution, otherSolutions) funcSig time

    interpret prop (as :: IO Result) >>= liftIO

runChecks :: MonadIO m => Environment -> RType -> UProgram -> FilterTest m Bool
runChecks env goalType prog =
  and <$> mapM (\f -> f modules funcSig body) checks
  where
    (modules, funcSig, body, _) = extractSolution env goalType prog

    checks = [ checkSolutionNotCrash
             , checkDuplicates]

checkSolutionNotCrash :: MonadIO m => [String] -> String -> String -> FilterTest m Bool
checkSolutionNotCrash modules sigStr body = liftIO executeCheck
  where
    handleNotSupported = (`catch` ((\ex -> print ex >> return True) :: NotSupportedException -> IO Bool))
    executeCheck = handleNotSupported $ do
      let funcSig = (instantiateSignature . parseTypeString) sigStr

      result <- validateSolution modules body funcSig defaultTimeoutMicro
      case result of
        Left err -> print err >> return True
        Right AlwaysFail -> return False
        Right _ -> return True

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

isHigherOrderArgument :: ArgumentType -> Bool
isHigherOrderArgument (ArgTypeFunc _ _) = True
isHigherOrderArgument (ArgTypeList sub) = isHigherOrderArgument sub
isHigherOrderArgument (ArgTypeApp l r) = any isHigherOrderArgument [l, r]
isHigherOrderArgument (ArgTypeTuple types) = any isHigherOrderArgument types
isHigherOrderArgument _ = False

filterHigherOrder_ :: FunctionSignature -> [String]
filterHigherOrder_ funcSig = map show $ filter isHigherOrderArgument $ _argsType funcSig

checkDuplicates :: MonadIO m => [String] -> String -> String -> FilterTest m Bool
checkDuplicates modules sigStr solution = do
  FilterState inputs solns <- get

  result <- liftIO $ compareSolution modules solution solns funcSig defaultTimeoutMicro

  case result of
    Left err -> do
      liftIO $ print err
      modify $ const FilterState {inputs = inputs, solutions = solution:solns}
      return True
    Right Failure{failingTestCase = c} -> do
      modify $ const FilterState {inputs = show c : inputs, solutions = solution:solns}
      return True
    _ -> return False

  where
    funcSig = (instantiateSignature . parseTypeString) sigStr
