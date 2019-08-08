module HooglePlus.FilterTest (runChecks, runChecks') where

import Language.Haskell.Interpreter
import Language.Haskell.Exts.Parser
import Text.Printf
import Control.Exception
import Data.List
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Test.QuickCheck
import qualified Data.Map as Map hiding (map, foldr)
import qualified Data.Set as Set hiding (map)
import System.Timeout
import Control.Monad
import Data.Maybe
import Data.Typeable

import HooglePlus.Utils
import Types.Environment
import Types.Program
import Types.Type hiding (typeOf)
import Types.Filtering
import Synquid.Type

excludeFunctions :: [String]
excludeFunctions = ["GHC.List.iterate", "GHC.List.repeat"]

rejectInfiniteFunctions :: Bool
rejectInfiniteFunctions = True

parseTypeString :: String -> FunctionSigniture
parseTypeString input = buildRet [] [] value
  where
    (ParseOk value) = parseType input

    buildRet constraints argList (TyForall _ _ (Just ctx) t) = buildRet constraints' argList t
      where constraints' = constraints ++ extractConstraints constraints ctx
    buildRet constraints argList (TyFun _ typeArg typeRet) = buildRet constraints argList' typeRet
      where argList' = argList ++ [extractType typeArg]
    buildRet constraints argList typeRet = (constraints, argList, extractType typeRet)

    extractType (TyVar _ (Ident _ name)) = Polymorphic name
    extractType (TyCon _ (UnQual _ (Ident _ name))) = Concrete name
    extractType (TyList _ arg) = ArgTypeList (extractType arg)
    extractType (TyParen _ t) = extractType t
    extractType other = throw $ NotSupportedException ("Not able to handle " ++ show other)

    extractQualified (ClassA _ (UnQual _ (Ident _ name)) var) =
      map (\x -> (extractType x, name)) var
    extractQualified (ParenA _ qual) = extractQualified qual
    extractQualified other = throw $ NotSupportedException ("Not able to extract " ++ show other)

    extractConstraints constraints (CxSingle _ item) = constraints ++ extractQualified item
    extractConstraints constraints (CxTuple _ list) = foldr ((++) . extractQualified) constraints list

generateRandomValue :: [String] -> String -> IO String
generateRandomValue imports typeName = do
  result <- runInterpreter $ do {
    setImports ("Test.QuickCheck":imports);

    act <- interpret ("do {\
    \it <- generate (arbitrary :: Gen (" ++ typeName ++ "));\
    \return $ show it\
    \}") (as :: IO String);

    liftIO act
  }

  case result of
    Left err -> putStrLn (displayException err) >> return "error \"unable to generate input\"" 
    Right res -> return ("(" ++ res ++ ")")

generateRandomValueWith :: Int -> Int -> [String] -> String -> IO String
generateRandomValueWith seed size imports typeName = do
  result <- runInterpreter $ do
    setImports ("Test.QuickCheck":imports)
    eval $ printf "unGen arbitrary (mkQCGen (%d)) (%d)" seed size
 
  case result of
    Left err -> putStrLn (displayException err) >> return "error \"unable to generate input\""
    Right res -> return $ printf "(%s)" res 


mapRandomParamsList :: [String] -> FunctionSigniture -> Int -> IO [String]
mapRandomParamsList imports env count = do
  base <- generateTestInput' imports env
  replicateM count (derive base)
  where
    derive :: [(String, String)] -> IO String
    derive base = do
      rand <- generate (arbitrary :: Gen Int)
      let index = rand `rem` length base
      let item = getNthElement index base

      replaced <- newRandomValue item
      let list = setNthElement index replaced base
      return $ unwords (map snd list)

    newRandomValue (name, _) = do
      val <- generateRandomValue imports name
      return (name, val)

    getNthElement idx list = item
      where (_, item:_) = splitAt idx list

    setNthElement idx item list
      | idx < 0   = list
      | otherwise = case splitAt idx list of
                    (front, _:back) -> front ++ item : back
                    _ -> list

-- generate test input as line of code
generateTestInput :: [String] -> FunctionSigniture -> IO String
generateTestInput imports env = do
  result <- generateTestInput' imports env
  return $ unwords (map snd result)

-- generate test input as pair (type, value)
generateTestInput' :: [String] -> FunctionSigniture -> IO [(String, String)]
generateTestInput' imports (constraints, args, retType) = mapM f (transformType constraints args)
  where
    f x = let str = show x in do
      val <- generateRandomValue imports str
      return (str, val)

    transformType [] args = args
    transformType (envItem:env) args = transformType env args'
      where
        args' = map (replace envItem) args

        -- replace all occurrance of nameL to its instantiated type
        replace (Polymorphic nameL, constraint) (Polymorphic nameR) =
          if nameL == nameR then
            case constraint of
              "Eq" -> Concrete "Int"
              "Num" -> Concrete "Int"
          else Polymorphic nameR
        replace constraint (ArgTypeList argType) = ArgTypeList (replace constraint argType)
        replace _ x = x

eval_ :: [String] -> String -> Int -> IO (Maybe (Either InterpreterError String))
eval_ modules line time = timeout time $ runInterpreter $ do
  setImports modules
  eval line

runChecks :: Environment -> RType -> UProgram -> IO Bool
runChecks env goalType prog = do
  list <- replicateM 10 (runChecks' modules funcSig body)
  return $ or list
  where
    args = _arguments env
    modules = "Prelude" : Set.toList (_included_modules env)
    argList = Map.toList args
    argNames = map fst argList
    argTypes = map snd argList
    monoGoals = map toMonotype argTypes
    funcSig = mkFunctionSigStr (monoGoals ++ [goalType])
    body = mkLambdaStr argNames prog

runChecks' :: [String] -> String -> String -> IO Bool
runChecks' modules funcSig body = if filterInf body then handleNotSupported $ do
  arg <- generateTestInput modules (parseTypeString funcSig)
  result <- eval_ modules (fmtFunction_ body funcSig arg) 3000000
  case result of
    Nothing -> putStrLn "Timeout in running always-fail detection" >> return True
    Just (Left err) -> putStrLn (displayException err) >> return False
    Just (Right res) -> putStrLn res >> return True
else pure $ not rejectInfiniteFunctions

filterInf :: String -> Bool
filterInf body = not $ any (`isInfixOf` body) excludeFunctions

-- *WIP: not used until the next iteration
checkDuplicate :: [String] -> String -> String -> String -> IO Bool
checkDuplicate modules funcSig bodyL bodyR = let fmt = fmtFunction_ in
  if not (filterInf bodyL) || not (filterInf bodyR) then pure $ not rejectInfiniteFunctions
  else do
    arg <- generateTestInput modules (parseTypeString funcSig) 
    retL <- eval_ modules (fmt bodyL funcSig arg) 3000000
    retR <- eval_ modules (fmt bodyR funcSig arg) 3000000

    case (retL, retR) of
      (Just (Right l), Just (Right r)) -> putStrLn "duplicate fcn" >> return (l /= r)
      _ -> return False

-- *WIP: not used until the next iteration
checkParamsUsage :: [String] -> String -> String -> Int -> IO [Maybe (Either InterpreterError String)]
checkParamsUsage modules funcSig body count = do
  args <- mapRandomParamsList modules (parseTypeString funcSig) count
  results <- mapM (\arg -> eval_ modules (fmtFunction_ body funcSig arg) 3000000) args
  return results

handleNotSupported = (`catch` ((\ex -> (putStrLn $ show ex) >> return True) :: NotSupportedException -> IO Bool))
fmtFunction_ = printf "((%s) :: %s) %s" :: String -> String -> String -> String