module Postfilter.FilterTest
  ( runChecks
  , checkSolutionNotCrash
  , checkDuplicates
  , generateIOPairs
  , parseTypeString
  ) where

import           Control.Exception              ( catch
                                                , throw
                                                )
import           Control.Monad                  ( when
                                                , zipWithM
                                                )
import           Control.Monad.State
import           Data.List                      ( intercalate
                                                , isInfixOf
                                                , nub
                                                )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import qualified Data.Set                      as Set
                                         hiding ( map )
import qualified Data.Text                     as Text
import           Language.Haskell.Exts.Parser   ( ParseResult(..)
                                                , parseType
                                                )
import           Language.Haskell.Exts.Syntax
import           System.Timeout                 ( timeout )
import           Text.Printf                    ( printf )

import           Test.SmallCheck                ( Depth )
import           Test.SmallCheck.Drivers
import           Text.Read                      (readMaybe)

import           Text.PrettyPrint.ANSI.Leijen   ( string )

import           HooglePlus.Utils
import           Paths_HooglePlus
import           Types.Common
import           Types.Environment
import           Types.Filtering
import           Types.Pretty
import           Types.Program
import           Types.Type              hiding ( typeOf )
import           Utility.Utils
import           Postfilter.GHCSocket
import Interpreter.Interpreter
import Interpreter.Session

-- import Debug.Trace

parseTypeString :: String -> FunctionSignature
parseTypeString input = FunctionSignature constraints argsType returnType
 where
  (constraints, argsType, returnType) = buildSig [] [] value
  value                     = case parseType input of
                                ParseOk v -> v
                                f -> error $ "parseTypeString: " ++ show f

  buildSig constraints argList (TyForall _ _ (Just ctx) t) = buildSig
    constraints'
    argList
    t
    where constraints' = constraints ++ extractConstraints constraints ctx
  buildSig constraints argList (TyFun _ typeArg typeRet) = buildSig
    constraints
    argList'
    typeRet
    where argList' = argList ++ [extractType typeArg]
  buildSig constraints argList (TyParen _ t) = buildSig constraints argList t
  buildSig constraints argList typeRet =
    (constraints, argList, extractType typeRet)

  extractType (TyVar _ (Ident  _ name          )) = Polymorphic name
  extractType (TyCon _ (UnQual _ (Ident _ name))) = Concrete name
  extractType (TyCon _ (Qual _ (ModuleName _ moduleName) (Ident _ id))) =
    Concrete (printf "%s.%s" moduleName id)
  extractType (TyList  _ arg    ) = ArgTypeList (extractType arg)
  extractType (TyParen _ t      ) = extractType t
  extractType (TyApp   _ l r    ) = ArgTypeApp (extractType l) (extractType r)
  extractType (TyTuple _ _ types) = ArgTypeTuple (map extractType types)
  extractType (TyFun _ src dst) =
    ArgTypeFunc (extractType src) (extractType dst)
  extractType other =
    throw $ NotSupportedException ("Not able to handle " ++ show other)

  extractQualified (TypeA  _ t   ) = [ClassConstraint (show $ extractType t)]
  extractQualified (ParenA _ qual) = extractQualified qual
  extractQualified other =
    throw $ NotSupportedException ("Not able to extract " ++ show other)

  extractConstraints constraints (CxSingle _ item) =
    constraints ++ extractQualified item
  extractConstraints constraints (CxTuple _ list) =
    foldr ((++) . extractQualified) constraints list
  extractConstraints constraints (CxEmpty _) = constraints

-- instantiate polymorphic types in function signature with `Int`
instantiateSignature :: FunctionSignature -> FunctionSignature
instantiateSignature (FunctionSignature _ argsType returnType) =
  FunctionSignature [] (map instantiate argsType) (instantiate returnType)
 where
  instantiate sig@(Concrete     name ) = sig
  instantiate (    Polymorphic  name ) = Concrete "Int"
  instantiate (    ArgTypeList  sub  ) = ArgTypeList $ instantiate sub
  instantiate (    ArgTypeTuple types) = ArgTypeTuple (map instantiate types)
  instantiate (ArgTypeApp l r) = ArgTypeApp (instantiate l) (instantiate r)
  instantiate (ArgTypeFunc (Polymorphic name) r) =
    ArgTypeFunc (Concrete "MyInt") (instantiate r)
  instantiate (ArgTypeFunc l r) = ArgTypeFunc (instantiate l) (instantiate r)

buildFunctionWrapper
  :: [(String, String)]
  -> String
  -> (String, String, String, String)
  -> Int
  -> String
buildFunctionWrapper functions solutionType params@(plain, typed, shows, unwrp) timeInMicro
  = unwords
    (  map (`buildLetFunction` solutionType) functions
    ++ [buildTimeoutWrapper (map fst functions) params timeInMicro]
    )
 where
  buildLetFunction :: (String, String) -> String -> String
  buildLetFunction (wrapperName, solution) solutionType =
    printf "let %s = ((%s) :: %s) in"
           wrapperName
           solution
           (replaceId "MyInt" "Int" solutionType) :: String

  buildTimeoutWrapper
    :: [String] -> (String, String, String, String) -> Int -> String
  buildTimeoutWrapper wrapperNames (plain, typed, shows, unwrp) timeInMicro =
    printf
      "let timeoutWrapper = \\%s -> (evaluateIO %d %s (Prelude.map (\\internal__f -> internal__f %s) [%s])) in"
      typed
      timeInMicro
      shows
      unwrp
      (intercalate ", " wrapperNames) :: String

buildNotCrashProp :: [String] -> String -> FunctionSignature -> String
buildNotCrashProp argNames solution funcSig = formatAlwaysFailProp params
                                                                   wrapper
 where
  params@(plain, typed, shows, unwrp) = showParams argNames (_argsType funcSig)

  wrapper = buildFunctionWrapper [("wrappedSolution", solution)]
                                 (show funcSig)
                                 params
                                 defaultTimeoutMicro

  formatAlwaysFailProp = formatProp "propAlwaysFail" "isFailedResult"
  -- formatNeverFailProp = formatProp "propNeverFail" "not <$> isFailedResult"

  formatProp
    :: String -> String -> (String, String, String, String) -> String -> String
  formatProp propName body (plain, typed, shows, unwrp) wrappedSolution =
    unwords
      [ wrappedSolution
      , printf
        "let %s %s = monadic (%s <$> Prelude.head <$> timeoutWrapper %s) in"
        propName
        plain
        body
        plain
      , printf "runStateT (smallCheckM %d (%s)) []" defaultDepth propName
      ]

buildDupCheckProp
  :: [String]
  -> (String, [String])
  -> FunctionSignature
  -> Int
  -> Depth
  -> [String]
buildDupCheckProp argNames (sol, otherSols) funcSig timeInMicro depth = map
  (\x -> buildDupCheckProp' argNames (sol, [x]) funcSig timeInMicro depth)
  otherSols

buildDupCheckProp'
  :: [String]
  -> (String, [String])
  -> FunctionSignature
  -> Int
  -> Depth
  -> String
buildDupCheckProp' argNames (sol, otherSols) funcSig timeInMicro depth =

  unwords [wrapper, formatProp]
 where
  params@(plain, typed, shows, unwrp) = showParams argNames (_argsType funcSig)
  solutionType                        = show funcSig

  wrapper =
    buildFunctionWrapper solutions (show funcSig) params defaultTimeoutMicro
  solutions = zip [ printf "result_%d" x :: String | x <- [0 ..] :: [Int] ]
                  (sol : otherSols)

  formatProp =
    unwords
      [ printf
        "let dupProp = existsUnique $ \\%s -> monadic (not <$> anyDuplicate <$> timeoutWrapper %s) in"
        plain
        plain
      , printf "runStateT (smallCheckM %d dupProp) []" depth
      ] :: String

evaluateProperty
  :: Sessions -> String -> IO (Either String SmallCheckResult)
evaluateProperty sessions property = do
  mbRes <- timeout defaultInterpreterTimeoutMicro $ execute sessions $ interpret property (as :: IO SmallCheckResult) >>= liftIO
  case mbRes of
    Nothing -> return $ Left "timeout"
    Just res -> case res of
      Left err -> return $ Left (show err)
      Right res' -> return $ Right res'

validateSolution
  :: Sessions
  -> [String]
  -> String
  -> FunctionSignature
  -> Int
  -> IO (Either String FunctionCrashDesc)
validateSolution sessions argNames solution funcSig time =
  evaluateResult' <$> evaluateProperty sessions alwaysFailProp
 where
  alwaysFailProp = buildNotCrashProp argNames solution funcSig

  evaluateSmallCheckResult
    :: Either String SmallCheckResult
    -> Either String SmallCheckResult
    -> Either String FunctionCrashDesc
  evaluateSmallCheckResult resultF resultS = case resultF of
    Left err | "timeout" `isInfixOf` err -> Right $ AlwaysFail $ caseToInput resultS
    Right (Nothing, _      )       -> Right $ AlwaysFail $ caseToInput resultS
    Right (_      , exF : _)       -> case resultS of
      Left "timeout" -> Right $ AlwaysSucceed exF
      Right (Nothing, _)             -> Right $ AlwaysSucceed exF

      Right (Just (CounterExample _ _), exS : _) ->
        Right $ PartialFunction $ Examples [exF, exS]
      _ -> error (show resultF ++ "???" ++ show resultS)
    _ -> Right $ AlwaysFail $ caseToInput resultS

  evaluateResult' result = case result of
    Left err | "timeout" `isInfixOf` err -> Right $ AlwaysFail $ Example [] "timeout"
    Left error -> Right $ AlwaysFail $ Example [] (show error)
    Right (Nothing, _       )      -> Right $ AlwaysFail $ caseToInput result
    Right (_      , examples)      -> Right $ PartialFunction $ Examples (take 2 examples) -- only need the first two examples, one succeed, one fail

  caseToInput :: Either String SmallCheckResult -> Example
  caseToInput (Right (_, example : _)) = example
  caseToInput _ = error "caseToInput: no example returned"

  preprocessOutput :: String -> String -> String
  preprocessOutput input output = fromMaybe "N/A" (listToMaybe selectedLine)
   where
    ios          = nub $ filter ([] /=) $ lines output
    selectedLine = filter (isInfixOf input) ios

compareSolution
  :: Sessions
  -> [String]
  -> String
  -> [String]
  -> FunctionSignature
  -> Int
  -> IO [Either String SmallCheckResult]
compareSolution sessions argNames solution otherSolutions funcSig time = do
  -- traceShow ("Checking properties" <+> pretty props) $ return ()
  mapM (evaluateProperty sessions) props
 where
  props = buildDupCheckProp argNames
                            (solution, otherSolutions)
                            funcSig
                            time
                            defaultDepth

runChecks
  :: MonadIO m
  => Sessions
  -> Environment
  -> TypeSkeleton
  -> TProgram
  -> FilterTest m (Maybe AssociativeExamples)
runChecks sessions env goalType prog = do
  notCrashRes <- checkSolutionNotCrash sessions argNames funcSig body
  if notCrashRes
    then do
      notDupRes <- checkDuplicates sessions argNames funcSig body

      state  <- get
      when notDupRes $ runPrints state

      return $ if notDupRes
        then Just (collectExamples (unqualifyFunc body, body) state)
        else Nothing
    else return Nothing
 where
  SynthesisResult funcSig body argNames = extractSolution env goalType prog

  runPrints state = do
    writeLog 2 "runChecks" "\n*******************FILTER*********************"
    writeLog 2 "runChecks" (pretty $ unqualifyFunc body)
    writeLog 2 "runChecks"
      $ string (printSolutionState (unqualifyFunc body, body) state)

checkSolutionNotCrash
  :: MonadIO m
  => Sessions
  -> [String]
  -> String
  -> TProgram
  -> FilterTest m Bool
checkSolutionNotCrash sessions argNames sigStr body = do
  fs@(FilterState _ _ examples _ _) <- get
  result                          <- liftIO executeCheck

  let pass = case result of
        Right (AlwaysFail _) -> False
        _                    -> True

  let Right desc = result
  put $ fs { solutionExamples = ((unqualifyFunc body, body), desc) : examples }
  return pass

 where
  handleNotSupported =
    (`catch` ((\ex -> return (Left (show ex))) :: NotSupportedException
               -> IO (Either String FunctionCrashDesc)
             )
    )
  funcSig      = (instantiateSignature . parseTypeString) sigStr
  executeCheck = handleNotSupported $ validateSolution sessions
                                                       argNames
                                                       (plainShow body)
                                                       funcSig
                                                       defaultTimeoutMicro


checkDuplicates
  :: MonadIO m
  => Sessions
  -> [String]
  -> String
  -> TProgram
  -> FilterTest m Bool
checkDuplicates sessions argNames sigStr solution = do
  fs@(FilterState is solns _ examples _) <- get
  case solns of

    -- no solution yet; skip the check
    [] -> do
      put fs { solutions = solution : solns }
      return True

    -- find an input i such that all synthesized results can be differentiated semantically
    _ -> do
      results <- liftIO $ compareSolution sessions
                                          argNames
                                          (plainShow solution)
                                          (map plainShow solns)
                                          funcSig
                                          defaultTimeoutMicro
      passTest <- and <$> zipWithM processResult results solns

      fs'@(FilterState is solns _ examples _) <- get
      when passTest $ writeLog 3 "checkDuplicates" $ "adding solution" <+> pretty solution
      if passTest then put fs' { solutions = solution : solns } else put fs

      return passTest

 where
  funcSig = (instantiateSignature . parseTypeString) sigStr
  caseToInput (Just (AtLeastTwo i_1 _ i_2 _)) = [i_1, i_2]
  caseToInput _ = error "caseToInput: expect AtLeastTwo"

  filterRelated i1 i2 (Example inputX _) = inputX == i1 || inputX == i2
  filterSuccess (Left err) | "timeout" `isInfixOf` err = False
  filterSuccess (Left _) = True
  filterSuccess (Right (r@(Just AtLeastTwo{}), newExamples)) = True
  filterSuccess _ = False

  processResult result otherSolution = do
    state@(FilterState is solns _ examples _) <- get
    case result of
      -- bypass the check on any timeout or error
      Left err | "timeout" `isInfixOf` err -> do
        writeLog 2 "processResult" $ "dup check timeout for" <+> pretty (solution, otherSolution)
        return False -- no example -> reject
      Left  err                      -> return True

      -- SmallCheck fails to find any differentiating input
      Right (Just NotExist, _)       -> return False
      Right (Nothing, _)             -> return False

      -- the trick to make SC to generate two inputs
      Right (r@(Just AtLeastTwo{}), newExamples) -> do
        let [i1, i2] = caseToInput r
        let sols =
              [ (unqualifyFunc solution     , solution)
              , (unqualifyFunc otherSolution, otherSolution)
              ]
        put $ state
          { filterInputs          = [i1, i2] ++ is
          , differentiateExamples =
            zip sols (filter (filterRelated i1 i2) newExamples) ++ examples
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
  typed = unwords $ map
    (\(idx, tipe) -> printf "(%s :: %s)" idx (show tipe) :: String)
    args'

  shows = "[" ++ intercalate ", " (formatIdx "(show %s)") ++ "]"
  formatIdx format = map ((printf format :: String -> String) . fst) args'

  replaceInner :: ArgumentType -> ArgumentType
  replaceInner x =
    let apply a b = ArgTypeApp (ArgTypeApp (Concrete "MyFun") a) b
    in  case x of
          Concrete     _      -> x
          Polymorphic  _      -> x
          ArgTypeList  t      -> ArgTypeList (replaceInner t)
          ArgTypeTuple ts     -> ArgTypeTuple (map replaceInner ts)
          ArgTypeApp  f   a   -> ArgTypeApp (replaceInner f) (replaceInner a)
          ArgTypeFunc arg res -> apply (replaceInner arg) (replaceInner res)

-- ******** Example Generator ********

generateIOPairs
  :: [Id]
  -> String
  -> FunctionSignature
  -> [String]
  -> Int
  -> Int
  -> Int
  -> Depth
  -> [String]
  -> [[String]]
  -> IO (Either String GeneratorResult)
generateIOPairs modules solution funcSig argNames numPairs timeInMicro interpreterTimeInMicro depth existingResults existingInputs
  = do
    res <- askGhcSocket property
    case res of
      Left err -> return $ Left err
      Right r -> case readMaybe r of
        Nothing -> error $ "[generateIOPairs]: cannot read " ++ r
        Just r' -> return $ Right r'
 where
  funcSig' = instantiateSignature funcSig
  typeStr  = show funcSig'
  params   = showParams argNames (_argsType funcSig')
  property = buildProp solution funcSig'

  buildProp :: String -> FunctionSignature -> String
  buildProp solution funcSig = formatProp params wrapper
   where
    wrapper = buildWrapper solution typeStr params defaultTimeoutMicro

    formatProp (plain, typed, shows, unwrp) wrappedSolution =
      unwords
        [ wrappedSolution
        , printf "let previousResults = [%s] in"
                 (intercalate ", " $ map show existingResults)
        , printf "let previousInputs = %s in" (show existingInputs)
        , printf
          "let prop %s = monadic ((wrappedSolution %s) >>= (waitState %d %s previousResults previousInputs)) in"
          plain
          plain
          numPairs
          shows
        , printf "execStateT (smallCheckM %d (exists prop)) []" depth
        ] :: String

  buildWrapper solution typeStr params timeInMicro = unwords
    [buildLetFunction solution typeStr, buildTimeoutWrapper params timeInMicro]
   where
    buildLetFunction :: String -> String -> String
    buildLetFunction solution solutionType =
      printf "let sol_wrappedSolution = ((%s) :: %s) in"
             solution
             (replaceId "MyInt" "Int" typeStr) :: String

    buildTimeoutWrapper :: (String, String, String, String) -> Int -> String
    buildTimeoutWrapper (plain, typed, shows, unwrp) timeInMicro =
      printf
        "let wrappedSolution %s = (liftIO $ Test.ChasingBottoms.timeOutMicro' %d (Test.ChasingBottoms.approxShow %d (sol_wrappedSolution %s))) in"
        typed
        timeInMicro
        defaultMaxOutputLength
        unwrp :: String
