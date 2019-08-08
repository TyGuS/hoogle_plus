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
  list <- replicateM defaultNumChecks (runChecks' modules funcSig body)
  return $ or list
  where
    (modules, funcSig, body, _) = extractSolution env goalType prog

runChecks' :: [String] -> String -> String -> IO Bool
runChecks' modules funcSig body = handleNotSupported executeCheck
  where
    executeCheck = do
      arg <- generateTestInput modules (parseTypeString funcSig)
      result <- eval_ modules (fmtFunction_ body funcSig arg) 3000000
      case result of
        Nothing -> putStrLn "Timeout in running always-fail detection" >> return True
        Just (Left err) -> putStrLn (displayException err) >> return False
        -- `seq` used to evaluate res, thus raise the exception on always-fail
        Just (Right res) -> seq res (pure ()) >> return True

handleNotSupported = (`catch` ((\ex -> print ex >> return True) :: NotSupportedException -> IO Bool))
fmtFunction_ = printf "((%s) :: %s) %s" :: String -> String -> String -> String