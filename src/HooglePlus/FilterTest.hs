{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns #-}
module HooglePlus.FilterTest where

import qualified Data.Map as Map
import qualified Hoogle
import qualified Language.Haskell.Interpreter as LHI
import qualified Test.QuickCheck as QC

import Language.Haskell.Interpreter (InterpreterT, InterpreterError(..), Extension(..), OptionVal(..))
import Control.Monad.Extra (andM)

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Bifunctor (second)
import Data.Either
import Data.List
import Data.Maybe
import Data.Typeable
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import System.Timeout
import Text.Printf
import Data.Containers.ListUtils (nubOrd)
import Data.List.Extra (splitOn)
import Data.UUID.V4 (nextRandom)

import HooglePlus.Utils (splitConsecutive, extractSolution, printFilter)
import Paths_HooglePlus
import Synquid.Type
import Types.Environment
import Types.Filtering
import Types.IOFormat (Example(Example))
import Types.Program
import Types.Type hiding (typeOf)
import PetriNet.Utils (unHTML)
import Synquid.Utils (getTmpDir)


import Debug.Trace

-- | Parse a string to an internal representation of type signatures.
-- >>> parseTypeString "(Eq a, Ord b) => a -> b"
-- (((Eq) (a)), ((Ord) (b))) => a -> b
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
    extractType other = throw $ NotSupportedException ("Type Parser: unsupported " ++ show other)

    extractQualified (TypeA _ t) = extractType t
    extractQualified (ParenA _ qual) = extractQualified qual
    extractQualified other = throw $ NotSupportedException ("Type Parser: unsupported " ++ show other)

    extractConstraints constraints (CxSingle _ item) = constraints ++ [extractQualified item]
    extractConstraints constraints (CxTuple _ list) = foldr ((++) . (:[]) . extractQualified) constraints list

-- | instantiate polymorphic types in function signature with `Int`
-- >>> instantiateSignature (parseTypeString "[a] -> a")
-- () => [Int] -> Int
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

buildFunctionWrapper :: [(String, String)] -> FunctionSignature -> (String, String, String, String) -> String
buildFunctionWrapper functions solutionType@FunctionSignature{_returnType} params@(plain, typed, shows, unwrp) =
    unwords
      (map (buildLetFunction $ show solutionType) functions ++ [buildWrapper (map fst functions) params (show _returnType)])
  where
    buildLetFunction :: String -> (String, String) -> String
    buildLetFunction programType (wrapperName, program) =
      printf "let %s = ((%s) :: %s) in" wrapperName program programType :: String

    -- ! the wrapper magic (i.e. MyInt) only lives here (inside `typed`)
    buildWrapper :: [String] -> (String, String, String, String) -> String -> String
    buildWrapper wrapperNames (plain, typed, shows, unwrp) retType =
      printf "let executeWrapper %s = (Prelude.map (\\f -> f %s :: %s) [%s]) in" typed unwrp retType (intercalate ", " wrapperNames) :: String

buildNotCrashProp :: String -> FunctionSignature -> String
buildNotCrashProp solution funcSig = formatNotCrashProp params wrapper
  where
    params@(plain, typed, shows, unwrp) = showParams (_argsType funcSig)

    wrapper = buildFunctionWrapper [("wrappedSolution", solution)] funcSig params
    formatNotCrashProp = formatProp "prop_not_crash" "\\out -> (not $ isFailedResult $ Prelude.head out) ==> True"

    formatProp propName propBody (plain, typed, shows, unwrp) wrappedSolution = unwords
      [ wrappedSolution
      , printf "let %s %s = monadicIO $ run $ labelEvaluation (%s) (executeWrapper %s) (%s) in" propName plain shows plain propBody
      , printf "quickCheckWithResult defaultTestArgs %s" propName ] :: String

buildDupCheckProp :: (String, [String]) -> FunctionSignature -> [String]
buildDupCheckProp (sol, otherSols) funcSig =
  map (\x -> buildDupCheckProp' (sol, [x]) funcSig) otherSols

buildDupCheckProp' :: (String, [String]) -> FunctionSignature -> String
buildDupCheckProp' (sol, otherSols) funcSig = unwords [wrapper, formatProp]
  where
    params@(plain, typed, shows, unwrp) = showParams (_argsType funcSig)
    solutionType = show funcSig

    wrapper = buildFunctionWrapper solutions funcSig params
    solutions = zip [printf "result_%d" x :: String | x <- [0..] :: [Int]] (sol:otherSols)

    formatProp = unwords
      [ printf "let prop_duplicate %s = monadicIO $ run $ labelEvaluation (%s) (executeWrapper %s) (\\out -> (not $ anyDuplicate out) ==> True) in" plain shows plain
      , printf "quickCheckWithResult defaultTestArgs prop_duplicate" ] :: String

-- | Run Hint with the default script loaded.
runInterpreterWithEnvTimeout :: Int -> InterpreterT IO a -> IO (Either InterpreterError a)
runInterpreterWithEnvTimeout timeInMicro = runInterpreterWithEnvTimeoutPrepareModule timeInMicro (sequence ioList)
  where ioList = [getDataFileName "InternalTypeGen.hs"]

-- | Run Hint with the default script loaded, but also prepare arguments for HOFs.
runInterpreterWithEnvTimeoutHOF :: MonadIO m => Int -> FunctionSignature -> InterpreterT IO a -> FilterTest m (Either InterpreterError a)
runInterpreterWithEnvTimeoutHOF timeInMicro funcSig executeInterpreter = do
    fileName <- prepareEnvironment funcSig

    let ioList = [getDataFileName "InternalTypeGen.hs", pure fileName]
    result <- liftIO $ runInterpreterWithEnvTimeoutPrepareModule timeInMicro (sequence ioList) executeInterpreter
    case result of
      Left (WontCompile e)  -> liftIO $ print e >> runInterpreterWithEnvTimeout timeInMicro executeInterpreter
      _                     -> return result

-- | Prepare all scripts, get the paths of scripts, and run Hint.
runInterpreterWithEnvTimeoutPrepareModule :: Int -> IO [String] -> InterpreterT IO a -> IO (Either InterpreterError a)
runInterpreterWithEnvTimeoutPrepareModule timeInMicro prepareModule executeInterpreter =
  mergeTimeout <$> timeout timeInMicro (prepareModule >>= runInterpreter)
  where
    runInterpreter moduleFiles = LHI.runInterpreter $ do
      LHI.set [LHI.languageExtensions := [ExtendedDefaultRules, ScopedTypeVariables]]
      LHI.loadModules moduleFiles
      LHI.getLoadedModules >>= LHI.setTopLevelModules
      executeInterpreter

    mergeTimeout = \case
      Just v  -> v
      Nothing -> Left $ NotAllowed "timeout"
      -- Hint only throws NotAllowed when setting a non-existing top-level module.
      -- We never had such cases and can safely steal NotAllowed to represent timeout. 

-- | Initiate an interpreter with context, evaluate the given property, and return the backend result. 
-- todo: refactor 'funcSig' out
evaluateProperties :: MonadIO m => [String] -> FunctionSignature -> [String] -> FilterTest m (Either InterpreterError [BackendResult])
evaluateProperties modules funcSig properties =
  runInterpreterWithEnvTimeoutHOF defaultInterpreterTimeoutMicro funcSig $ do
    LHI.setImportsQ (zip modules (repeat Nothing) ++ frameworkModules)
    mapM (\prop -> LHI.interpret prop (LHI.as :: IO BackendResult) >>= liftIO) properties

validateCandidate :: MonadIO m => [String] -> Candidate -> FunctionSignature -> FilterTest m CandidateValidDesc
validateCandidate modules solution funcSig = do
    result <- evaluateProperties modules funcSig [prop]
    case result of
      Left  (NotAllowed _)            -> return $ Unknown "timeout"
      Left  error                     -> return $ Unknown $ show error
      Right [result]                  -> return $ readResult result
  where
    prop = buildNotCrashProp solution funcSig

    readResult :: BackendResult -> CandidateValidDesc
    readResult r@QC.GaveUp{QC.numTests}
              | numTests == 0 = Invalid
              | otherwise     = Partial $ parseExamples r
    readResult r@QC.Success{} = Total $ parseExamples r

classifyCandidate :: MonadIO m => [String] -> Candidate -> FunctionSignature -> [Candidate] -> FilterTest m CandidateDuplicateDesc
classifyCandidate modules candidate funcSig previousCandidates = if null previousCandidates then return (New []) else do
    let properties    =   buildDupCheckProp (candidate, previousCandidates) funcSig
    interpreterResult <-  evaluateProperties modules funcSig properties

    let examples      = readInterpreterResult candidate previousCandidates interpreterResult
    
    if all isJust examples
      then return $ New         (mergeExamples $ map fromJust examples)
      else return $ DuplicateOf (fst $ head $ filter snd $ zip previousCandidates $ map isJust examples)
  where
    readResult :: Candidate -> Candidate -> BackendResult -> Maybe AssociativeExamples
    readResult candidate previousCandidate result = case result of
        QC.Failure {}                           -> Nothing
        QC.GaveUp {QC.numTests} | numTests == 0 -> Nothing
        QC.GaveUp {}                            -> assocs
        QC.Success {}                           -> assocs
      where
        (examples, examplesForPrev) = splitConsecutive $ parseExamples result
        assocs = Just [(candidate, examples), (candidate, examplesForPrev)]

    readInterpreterResult :: Candidate -> [Candidate] -> Either InterpreterError [BackendResult] -> [Maybe AssociativeExamples]
    readInterpreterResult candidate previousCandidates =
      \case
        Left (NotAllowed _) -> [Nothing]
        Left err            -> []
        Right results       -> zipWith (readResult candidate) previousCandidates results

    mergeExamples :: [AssociativeExamples] -> AssociativeExamples
    mergeExamples rawExamples = 
      let exampleMaps = map (Map.fromList . map (second (take 2))) rawExamples in
        Map.toList $ Map.unionsWith (++) exampleMaps

runChecks :: MonadIO m => Environment -> TypeSkeleton -> UProgram -> FilterTest m (Maybe AssociativeExamples)
runChecks env goalType prog = do
  result <- andM $ map (\f -> f modules funcSig body) checks

  state <- get
  if result
    then liftIO $ printFilter body state
    else modify $ \s -> s {discardedSolutions = body : discardedSolutions s}

  -- add extra examples from solution descriptions
  let extraExamples = maybe Map.empty (Map.fromList . (:[]) . toExample) $ find ((==) body . fst) $ solutionDescriptions state
  let assocExamples = Map.toList $ Map.unionWith (++) (differentiateExamples state) extraExamples
  return $ if result then Just assocExamples else Nothing
  where
    (modules, funcSigStr, body, _) = extractSolution env goalType prog
    checks = [ checkSolutionNotCrash
             , checkDuplicates]

    funcSig = (instantiateSignature . parseTypeString) funcSigStr

    toExample :: (String, CandidateValidDesc) -> (String, [Example])
    toExample (s, v) = ((,) s) $ take defaultNumExtraExamples $ case v of Total xs -> xs; Partial xs -> xs; _ -> []

checkSolutionNotCrash :: MonadIO m => [String] -> FunctionSignature -> String -> FilterTest m Bool
checkSolutionNotCrash modules funcSig solution = do
  result <- validateCandidate modules solution funcSig
  modify $ \s -> s {solutionDescriptions = (solution, result) : solutionDescriptions s }
  return $ isSuccess result
      


checkDuplicates :: MonadIO m => [String] -> FunctionSignature -> String -> FilterTest m Bool
checkDuplicates modules funcSig candidate = do
  FilterState _ previousCandidates _ _ _ _ <- get
  result <- classifyCandidate modules candidate funcSig previousCandidates

  case result of
    DuplicateOf _ -> return False
    New assocExamples -> do
      modify $ \s -> s {
        solutions             = candidate : solutions s,
        differentiateExamples = Map.unionWith (++) (differentiateExamples s) (Map.fromList assocExamples)
      }

      return True


  -- case solns_ of
  --   -- base case: skip the check for the first solution
  --   [] -> modify (\s -> s {solutions = [solution]}) >> return True

  --   -- inductive: find an input i such that all synthesized results can be differentiated semantically
  --   _ -> do

  --     checkResult <- classifyCandidate modules solution 
      
  --     -- interpreterResult <- compareSolution modules solution solns_ funcSig
  --     -- examples          <- readInterpreterResult solns_ interpreterResult

  --     -- let isSucceed =   all isJust examples
  --     -- if  not isSucceed then return False else do
  --     --   let examples' = Map.unionsWith (++) $ map (Map.fromList . fromJust) examples
  --     --   modify $ \s -> s {
  --     --     solutions             = solution : solutions s,
  --     --     differentiateExamples = Map.unionWith (++) (differentiateExamples s) examples'
  --     --   }

  --       return True

-- show parameters to some Haskell representation
-- (plain variables, typed variables, list of show, unwrapped variables)
-- >> showParams ["Int"] => ("arg_0", "(arg_0 :: MyInt)", "[show arg_0]", "(unwrap arg_0)")
showParams :: [ArgumentType] -> (String, String, String, String)
showParams args = (plain, typed, shows, unwrp)
  where
    args' = zip [1..] $ map replaceMyType args

    plain = unwords $ formatIdx "(arg_%d)"
    unwrp = unwords $ formatIdx "(unwrap arg_%d)"
    typed = unwords $ map (\(idx, tipe) -> printf "(arg_%d :: %s)" idx (show tipe) :: String) args'

    shows = "[" ++ intercalate ", " (formatIdx "(show arg_%d)") ++ "]"
    formatIdx format = map ((printf format :: Int -> String) . fst) args'

replaceMyType :: ArgumentType -> ArgumentType
replaceMyType x =
  let apply a b = ArgTypeApp (ArgTypeApp (Concrete "MyFun") a) b in case x of
    Concrete      "Int"     -> Concrete "MyInt"
    Concrete      "Char"    -> Concrete "MyChar"
    Concrete      "String"  -> ArgTypeList $ Concrete "MyChar"
    Concrete      _         -> x
    Polymorphic   _         -> x
    ArgTypeList   t         -> ArgTypeList (replaceMyType t)
    ArgTypeTuple  ts        -> ArgTypeTuple (map replaceMyType ts)
    ArgTypeApp    f a       -> ArgTypeApp (replaceMyType f) (replaceMyType a)
    ArgTypeFunc   arg res   -> apply (replaceMyType arg) (replaceMyType res)

-- parse Example string from QC.label to Example
parseExamples :: BackendResult -> [Example]
parseExamples result = concat $ map (read . head) $ Map.keys $ QC.labels result

extractHigherOrderQuery :: FunctionSignature -> [ArgumentType]
extractHigherOrderQuery FunctionSignature{_constraints, _argsType, _returnType} = nub $ concatMap (`extract` []) _argsType
  where
    extract :: ArgumentType -> [ArgumentType] -> [ArgumentType]
    extract argType xs = case argType of
      ArgTypeFunc   l r   -> argType : xs
      ArgTypeTuple  types -> ((concatMap (`extract` []) types) ++ xs)
      ArgTypeList   sub   -> (extract sub []) ++ xs
      ArgTypeApp    l r   -> ((extract l []) ++ (extract r []) ++ xs)
      _                   -> xs

queryHoogle :: [String] -> IO [[String]]
-- queryHoogle types = return []
queryHoogle types = Hoogle.defaultDatabaseLocation >>= (`Hoogle.withDatabase` invokeQuery)
  where invokeQuery db = do
              let functions = map ( take 5 .
                                  nubOrd .
                                  map ( head .
                                        splitOn " :: " .
                                        unHTML .
                                        Hoogle.targetItem
                                      ) .
                                  filter isInModuleList .
                                  filter doesNotHaveTypeClass .
                                  Hoogle.searchDatabase db .
                                  (++) ":: "
                                ) types
              return functions
        isInModuleList x = let Just (name, _) = Hoogle.targetModule x in name `elem` hoogleQueryModuleList
        doesNotHaveTypeClass Hoogle.Target{Hoogle.targetItem} = not ("=&gt;" `isInfixOf` targetItem)

queryHooglePlus :: [String] -> IO [[String]]
queryHooglePlus types = print types >> print "called" >> return []

queryHigherOrderArgument :: MonadIO m => [String] -> FilterTest m [[String]]
queryHigherOrderArgument queries = do
    FilterState _ _ _ _ _ cache <- get
    let cachedResults = zip queries $ map (`Map.lookup` cache) queries

    let nextQueries = map fst $ filter needsQuery cachedResults
    if null nextQueries
      then return $ map (fromJust . snd) $ cachedResults
      else do
            nextQueryResults <- Map.unionsWith (++) <$> mapM (\f -> Map.fromList <$> zip nextQueries <$> (liftIO $ f nextQueries)) searchFunctions

            let cachedResultMap = Map.fromList $ map (\(q, r) -> (q, fromJust r)) $ filter (not . needsQuery) cachedResults
            let result = Map.union cachedResultMap nextQueryResults

            modify (\state -> state { higherOrderArgumentCache = Map.union (higherOrderArgumentCache state) result })
            return (map snd $ Map.toList result)
  where
    needsQuery (_, Nothing) = True
    needsQuery _            = False

    searchFunctions = [queryHoogle, queryHooglePlus]


-- >>> prepareEnvironment $ parseTypeString "a -> ([Int] -> Int) -> (a -> [a]) -> a"
-- >>> runInterpreterWithEnvTimeoutHOF (10^8) (parseTypeString "a -> (Int -> [Int]) -> a") (return "114514")
prepareEnvironment :: MonadIO m => FunctionSignature -> FilterTest m String
prepareEnvironment funcSig = do
    let higherOrderTypes = extractHigherOrderQuery funcSig
    queryResults <- queryHigherOrderArgument $ map show higherOrderTypes

    tmpDir <- liftIO $ getTmpDir
    baseName <- liftIO $ nextRandom
    let baseNameStr = show baseName ++ ".hs"
    let fileName = tmpDir ++ "/" ++ baseNameStr

    let sourceCode = buildEnvFileContent higherOrderTypes queryResults
    if null queryResults || all null queryResults
      then liftIO $ writeFile fileName ""
      else liftIO $ writeFile fileName sourceCode

    return fileName
  where
    prelude = [ "{-# LANGUAGE FlexibleInstances #-}"
              , "module HigherOrderParamsEnv where"
              , "import Control.Monad"
              , "import Test.QuickCheck"
              , "import InternalTypeGen"
              ] ++ map ((++) "import ") hoogleQueryModuleList

    postlude = "instance Arbitrary (%s) where arbitrary = sized $ \\n -> if n < %d then elements insFunc_%d else liftM Generated arbitrary"
    buildInstanceFunction tipe expr = printf "(Expression \"%s\" (%s))" expr (buildExpression tipe expr) :: String
    buildInstanceFunctions tipe exprs = "[" ++ (intercalate ", " $ map (buildInstanceFunction tipe) exprs) ++ "]"

    buildEnvFileContent types exprGroups = unlines $ prelude ++ (concat $ zipWith3 buildStep [(1 :: Int)..] types exprGroups)
      where
        buildStep _ tipe []    = ["-- no Hoogle result available; bypassed building " ++ show tipe ]
        buildStep i tipe exprs = [ printf "insFunc_%d = %s" i (buildInstanceFunctions tipe exprs)
                                 , printf postlude (buildAppType $ replaceMyType tipe) higherOrderGenMaxSize i
                                 ] :: [String]

    buildAppType :: ArgumentType -> String
    buildAppType = \case
            ArgTypeFunc l r   -> printf "MyFun (%s) (%s)" (buildAppType l) (buildAppType r) 
            otherType         -> show otherType

    buildExpression :: ArgumentType -> String -> String
    buildExpression tipe expr = unwords (["\\arg1 ->"] ++ [printf "wrap $ \\arg%d ->" x | x <- [2..depth] :: [Int]] ++ [expr, unwords [printf "(unwrap arg%d)" x | x <- [1..depth] :: [Int]]])
      where
        depth = getDepth 0 tipe
        getDepth i = \case
                        ArgTypeFunc _ r -> 1 + getDepth i r
                        _ -> 0

-- ******** Example Generator ********
generateIOPairs :: [String] -> String -> FunctionSignature -> Int -> Int -> Int -> [String] -> [[String]] -> IO (Either InterpreterError GeneratorResult)
generateIOPairs modules solution funcSig numPairs timeInMicro interpreterTimeInMicro existingResults existingInputs = error "not implemented"
