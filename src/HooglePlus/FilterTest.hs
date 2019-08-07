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
import Synquid.Type

excludeFunctions :: [String]
excludeFunctions = ["GHC.List.iterate"]

rejectInfiniteFunctions :: Bool
rejectInfiniteFunctions = True

data ArgumentType = Concrete String
                  | Polymorphic String
                  | ArgTypeList ArgumentType
                    deriving (Eq)

instance Show ArgumentType where
  show (Concrete name) = name
  show (Polymorphic _) = "Int"
  show (ArgTypeList sub) = printf "[%s]" (show sub)

data NotSupportedException = NotSupportedException String
  deriving (Show, Typeable)

instance Exception NotSupportedException

type TypeEnv = [(ArgumentType, String)]

parseTypeString :: String -> (TypeEnv, [ArgumentType], ArgumentType)
parseTypeString input = buildRet [] [] value
  where
    (ParseOk value) = parseType input

    argName (TyVar _ (Ident _ name)) = Polymorphic name
    argName (TyCon _ (UnQual _ (Ident _ name))) = Concrete name
    argName (TyList _ arg) = ArgTypeList (argName arg)
    argName (TyParen _ t) = argName t
    argName other = throw $ NotSupportedException ("Not able to handle " ++ show other)

    buildRet typeEnv argList (TyForall _ _ (Just ctx) t) = buildRet typeEnv' argList t
      where typeEnv' = typeEnv ++ buildEnv typeEnv ctx
    buildRet typeEnv argList (TyFun _ typeArg typeRet) = buildRet typeEnv argList' typeRet
      where argList' = argList ++ [argName typeArg]
    buildRet typeEnv argList typeRet = (typeEnv, argList, argName typeRet)

    extractQualified (ClassA _ (UnQual _ (Ident _ name)) var) =
      map (\x -> (argName x, name)) var
    extractQualified (ParenA _ qual) = extractQualified qual
    extractQualified other = throw $ NotSupportedException ("Not able to extract " ++ show other)
    buildEnv typeEnv (CxSingle _ item) = typeEnv ++ extractQualified item
    buildEnv typeEnv (CxTuple _ list) = foldr ((++) . extractQualified) typeEnv list

generateRandomValue :: String -> IO String
generateRandomValue typeName = do
  result <- runInterpreter $ do {
    setImports ["Prelude", "Test.QuickCheck"];

    act <- interpret ("do {\
    \it <- generate (arbitrary :: Gen (" ++ typeName ++ "));\
    \return $ show it\
    \}") (as :: IO String);

    liftIO act
  }
  case result of
    Left err -> putStrLn (displayException err) >> return "error \"114514\""
    Right res -> return ("(" ++ res ++ ")")

mapRandomMultiple :: (TypeEnv, [ArgumentType], ArgumentType) -> Int -> IO [String]
mapRandomMultiple env count = do
  base <- mapRandomArguments' env
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
      val <- generateRandomValue name
      return (name, val)

    getNthElement idx list = item
      where (_, item:_) = splitAt idx list

    setNthElement idx item list
      | idx < 0   = list
      | otherwise = case splitAt idx list of
                    (front, _:back) -> front ++ item : back
                    _ -> list

mapRandomArguments :: (TypeEnv, [ArgumentType], ArgumentType) -> IO String
mapRandomArguments env = do
  result <- mapRandomArguments' env
  return $ unwords (map snd result)

mapRandomArguments' :: (TypeEnv, [ArgumentType], ArgumentType) -> IO [(String, String)]
mapRandomArguments' (typeEnv, args, retType) = mapM f (transformType typeEnv args)
  where
    f x = let str = show x in do
      val <- generateRandomValue str
      return (str, val)

    transformType [] args = args
    transformType (envItem:env) args = transformType env args'
      where
        args' = map (replace envItem) args

        replace (Polymorphic name, cons) (Polymorphic name') =
          if name == name' then
            case cons of
              "Eq" -> Concrete "Int"
              "Num" -> Concrete "Int"
          else Polymorphic name'
        replace cons (ArgTypeList argType) = ArgTypeList (replace cons argType)
        replace _ x = x

eval_ :: [String] -> String -> Int -> IO (Maybe (Either InterpreterError String))
eval_ modules line time = timeout time $ runInterpreter $ do {
  setImports modules;
  eval line
}

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
  arg <- (mapRandomArguments . parseTypeString) funcSig
  result <- eval_ modules (fmtFunction_ body funcSig arg) 3000000
  case result of
    Nothing -> putStrLn "Timeout in running always-fail detection" >> return True
    Just (Left err) -> putStrLn (displayException err) >> return False
    Just (Right res) -> putStrLn res >> return True
else pure $ not rejectInfiniteFunctions

filterInf :: String -> Bool
filterInf body = not $ any (`isInfixOf` body) excludeFunctions

checkDuplicate :: [String] -> String -> String -> String -> IO Bool
checkDuplicate modules funcSig bodyL bodyR = let fmt = fmtFunction_ in
  if not (filterInf bodyL) || not (filterInf bodyR) then pure $ not rejectInfiniteFunctions 
  else do
    arg <- (mapRandomArguments . parseTypeString) funcSig
    retL <- eval_ modules (fmt bodyL funcSig arg) 3000000
    retR <- eval_ modules (fmt bodyR funcSig arg) 3000000

    case (retL, retR) of
      (Just (Right l), Just (Right r)) -> putStrLn "duplicate fcn" >> return (l /= r)
      _ -> return False

checkParamsUsage :: [String] -> String -> String -> Int -> IO [Maybe (Either InterpreterError String)]
checkParamsUsage modules funcSig body count = do
  args <- mapRandomMultiple (parseTypeString funcSig) count
  results <- mapM (\arg -> eval_ modules (fmtFunction_ body funcSig arg) 3000000) args
  return results

handleNotSupported = (`catch` ((\ex -> (putStrLn $ show ex) >> return True) :: NotSupportedException -> IO Bool))
fmtFunction_ = printf "((%s) :: %s) %s" :: String -> String -> String -> String