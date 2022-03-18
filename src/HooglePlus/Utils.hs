module HooglePlus.Utils where

import           Control.Monad.State            ( MonadState(get)
                                                , StateT
                                                , evalStateT
                                                , modify
                                                )
import qualified CoreSyn                       as Syn
import           Data.Function                  ( on )
import           Data.List                      ( groupBy
                                                , intercalate
                                                , isInfixOf
                                                , isPrefixOf
                                                , isSuffixOf
                                                , sortOn
                                                )
import           Data.List.Extra                ( dropEnd
                                                , nubOrdOn
                                                )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isJust )
import qualified Data.Set                      as Set
                                         hiding ( map )
import qualified Data.Text                     as Text
import           GHC.Paths                      ( libdir )
import qualified Language.Haskell.Interpreter  as LHI
import           SimplCore                      ( core2core )
import           System.Directory               ( removeFile )
import           Text.Printf                    ( printf )
import           Text.Regex                     ( Regex
                                                , matchRegex
                                                , mkRegex
                                                , subRegex
                                                )

import           Types.Environment
import           Types.Filtering
import           Types.IOFormat                 ( Example(Example) )
import qualified Types.IOFormat                as IOFormat
import           Types.Program
import           Types.Type
import           Utility.Utils

-- Converts the list of param types into a haskell function signature.
-- Moves typeclass-looking things to the front in a context.
mkFunctionSigStr :: [TypeSkeleton] -> String
mkFunctionSigStr args = addConstraints
  $ Prelude.foldr accumConstraints ([], []) args
 where
  showSigs = intercalate " -> "
  wrapParen x = "(" ++ x ++ ")"
  addConstraints ([], baseSigs) = showSigs baseSigs
  addConstraints (constraints, baseSigs) =
    "(" ++ intercalate ", " constraints ++ ") => " ++ showSigs baseSigs

  accumConstraints
    :: TypeSkeleton -> ([String], [String]) -> ([String], [String])
  accumConstraints (DatatypeT id [TypeVarT tyvarName]) (constraints, baseSigs)
    | tyclassPrefix `Text.isPrefixOf` id
    = let classNameRegex =
            mkRegex $ Text.unpack tyclassPrefix ++ "([a-zA-Z]*)"
          className  = subRegex classNameRegex (Text.unpack id) "\\1"
          constraint = unwords [className, Text.unpack tyvarName]
      in  (constraint : constraints, baseSigs)
  accumConstraints otherTy (constraints, baseSigs) =
    let otherStr = if isFunctionType otherTy
          then wrapParen (show otherTy)
          else show otherTy
    in  (constraints, otherStr : baseSigs)

-- mkLambdaStr produces a oneline lambda expr str:
-- (\x y -> body))
mkLambdaStr :: [String] -> TProgram -> String
mkLambdaStr args body =
  let nontcArgs = filter (not . (Text.unpack tyclassArgBase `isPrefixOf`)) args
      argStr = unwords nontcArgs
      unTypeclassed = toHaskellSolution (show body)
  in  printf "\\%s -> %s" argStr unTypeclassed

mkLambda :: [String] -> TProgram -> TProgram
mkLambda args body =
  let nontcArgs = filter (not . (Text.unpack tyclassArgBase `isPrefixOf`)) args
      unTypeclassed = untypeclass body
  in  foldr (\x p -> untyped (PFun (Text.pack x) p)) unTypeclassed nontcArgs

toHaskellSolution :: String -> String
toHaskellSolution bodyStr =
  let oneLineBody   = unwords $ lines bodyStr
      noTypeclasses = removeTypeclasses oneLineBody
  in  noTypeclasses

removeAll :: Regex -> String -> String
removeAll a b = unwords $ words $ go a b
 where
  go regex input = if isJust $ matchRegex regex input
    then go regex $ subRegex regex input ""
    else input

removeTypeclassArgs :: String -> String
removeTypeclassArgs =
  removeAll (mkRegex (Text.unpack tyclassArgBase ++ "[0-9]+"))

removeTypeclassInstances :: String -> String
removeTypeclassInstances =
  removeAll (mkRegex (Text.unpack tyclassInstancePrefix ++ "[0-9]*[a-zA-Z]*"))

removeTypeclasses :: String -> String
removeTypeclasses =
  removeEmptyParens . removeTypeclassArgs . removeTypeclassInstances
  where removeEmptyParens = removeAll (mkRegex "\\(\\ +\\)")

printSolution :: String -> IO ()
printSolution solution = do
  putStrLn "*******************SOLUTION*********************"
  putStrLn $ "SOLUTION: " ++ toHaskellSolution (show solution)
  putStrLn "************************************************"

collectExamples :: SolutionPair -> FilterState -> AssociativeExamples
collectExamples solution (FilterState _ sols samples examples) =
  map mkGroup
    $  groupBy (\x y -> fst x == fst y)
    $  sortOn fst
    $  examples ++ checkedExs
 where
  [(_, desc)] = filter ((== solution) . fst) samples
  checkedExs  = zip (repeat solution) (descToExample desc)
  mkGroup xs = (fst (head xs), nubOrdOn IOFormat.inputs $ map snd xs)

descToExample :: FunctionCrashDesc -> [Example]
descToExample (AlwaysSucceed   ex ) = [ex]
descToExample (AlwaysFail      ex ) = [ex]
descToExample (PartialFunction exs) = exs
descToExample _                     = []

printSolutionState :: SolutionPair -> FilterState -> String
printSolutionState solution (FilterState _ sols workingExamples diffExamples) =
  unlines [ios, diffs]
 where
  ios =
    let [(_, desc)] = filter ((== solution) . fst) workingExamples in show desc
  diffs =
    let examples = groupBy ((==) `on` fst) (sortOn fst diffExamples)
    in  unlines (map showGroup examples)

  showGroup :: [(SolutionPair, Example)] -> String
  showGroup xs = unlines (show (fst $ head xs) : map (show . snd) xs)

data SynthesisResult = SynthesisResult
  { getTypeQuery  :: String
  , getSynProgram :: TProgram
  , getArgNames   :: [String]
  }

extractSolution :: Environment -> TypeSkeleton -> TProgram -> SynthesisResult
extractSolution env goalType prog = SynthesisResult funcSig body argNames
 where
  argList   = getArguments env
  argNames  = map (Text.unpack . fst) argList
  argTypes  = map snd argList
  monoGoals = map toMonotype argTypes
  funcSig   = mkFunctionSigStr (monoGoals ++ [goalType])
  body      = mkLambda argNames prog

updateEnvWithBoundTyVars
  :: SchemaSkeleton -> Environment -> (Environment, TypeSkeleton)
updateEnvWithBoundTyVars (Monotype ty) env = (env, ty)
updateEnvWithBoundTyVars (ForallT x ty) env =
  updateEnvWithBoundTyVars ty (addTypeVar x env)

updateEnvWithSpecArgs
  :: TypeSkeleton -> Environment -> (Environment, TypeSkeleton)
updateEnvWithSpecArgs (FunctionT x tArg tRes) env =
  let (env', ret) = updateEnvWithSpecArgs tRes env
  in  (addComponent x (Monotype tArg) $ addArgument x tArg env', ret)
updateEnvWithSpecArgs ty env = (env, ty)

preprocessEnvFromGoal :: Goal -> (Environment, TypeSkeleton)
preprocessEnvFromGoal goal = updateEnvWithSpecArgs monospec env'
 where
  env              = gEnvironment goal
  (env', monospec) = updateEnvWithBoundTyVars (gSpec goal) env

matchNiceFunctions :: String -> StateT [(String, String)] IO String
matchNiceFunctions prog | null prog                            = return prog
matchNiceFunctions prog | head prog == '[' && last prog == ']' = do
  st <- get
  case lookup prog st of
    Just p  -> return p
    Nothing -> do
      let progElmts = dropEnd 1 $ drop 1 prog
      let sepElmts  = splitOn "," progElmts
      convertedElmts <- mapM matchNiceFunctions sepElmts
      let newProg = printf "[%s]" (intercalate "," convertedElmts)
      modify ((prog, newProg) :)
      return newProg
matchNiceFunctions prog | '\\' `elem` prog && "->" `isInfixOf` prog = do
  st <- get
  case lookup prog st of
    Just p  -> return p
    Nothing -> do
      let inputs       = [-1, 0, 1, 2]
      let concatInputs = [[], [0], [0, 0], [1]]
      let concatOutput = [[], [0, 0], [0, 0, 0, 0], [1, 1]]
      let prog' =
            if "..." `isInfixOf` prog then replaceId "..." "" prog else prog
      let stmt = printf "GHC.List.map (%s) %s" prog' (show inputs)
      result  <- runStmt stmt
      newProg <- case result of
        "[-3,0,3,6]" -> return "\\x -> x * 3"
        "[0,1,2,3]"  -> return "\\x -> x + 1"
        "[1,0,1,4]"  -> return "\\x -> x * x"
        _            -> do
          result2 <- runStmt
            $ printf "GHC.List.map (%s) %s" prog' (show concatInputs)
          if result2 == show concatOutput
            then return "\\x -> x ++ x"
            else do
              result3 <- runStmt $ printf
                "(GHC.List.all ((==) (GHC.List.head %s)) %s, GHC.List.head %s)"
                result
                result
                result
              if take 5 result3 == "(True"
                then return $ printf "const %s" (dropEnd 1 $ drop 6 result3)
                else return prog
      modify ((prog, newProg) :)
      return newProg
 where
  runStmt p = do
    let mdls =
          [ "Data.Maybe"
          , "GHC.List"
          , "Data.List"
          , "Data.Eq"
          , "GHC.Char"
          , "Data.Function"
          ]
    result <- LHI.runInterpreter $ do
      LHI.setImports mdls
      -- allow extensions for function execution
      extensions <- LHI.get LHI.languageExtensions
      LHI.set
        [ LHI.languageExtensions
            LHI.:= ( LHI.ExtendedDefaultRules
                   : LHI.ScopedTypeVariables
                   : extensions
                   )
        ]
      LHI.eval p
    return $ either show id result
matchNiceFunctions prog = return prog

niceInputs :: Example -> IO Example
niceInputs (Example ins out) = do
  ins' <- evalStateT (mapM matchNiceFunctions ins) []
  return (Example ins' out)
