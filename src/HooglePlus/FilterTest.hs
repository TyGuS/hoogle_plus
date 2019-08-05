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

import HooglePlus.Utils
import Types.Environment
import Types.Program
import Types.Type hiding (typeOf)
import Synquid.Type

excludeFunctions :: [String]
excludeFunctions = ["GHC.List.iterate"]

data ArgumentType = Concrete String
                  | Polymorphic String
                  | ArgTypeList ArgumentType
                    deriving (Eq)

instance Show ArgumentType where
  show (Concrete name) = name
  show (Polymorphic _) = "Int"
  show (ArgTypeList sub) = printf "[%s]" (show sub)

type TypeEnv = [(ArgumentType, String)]

parseTypeString :: String -> (TypeEnv, [ArgumentType], ArgumentType)
parseTypeString input = buildRet [] [] value
  where
    (ParseOk value) = parseType input

    argName (TyVar _ (Ident _ name)) = Polymorphic name
    argName (TyCon _ (UnQual _ (Ident _ name))) = Concrete name
    argName (TyList _ arg) = ArgTypeList (argName arg)
    argName (TyParen _ t) = argName t

    buildRet typeEnv argList (TyForall _ _ (Just ctx) t) = buildRet typeEnv' argList t
      where typeEnv' = typeEnv ++ buildEnv typeEnv ctx
    buildRet typeEnv argList (TyFun _ typeArg typeRet) = buildRet typeEnv argList' typeRet
      where argList' = argList ++ [argName typeArg]
    buildRet typeEnv argList typeRet = (typeEnv, argList, argName typeRet)

    extractQualified (ClassA _ (UnQual _ (Ident _ name)) var) =
      map (\x -> (argName x, name)) var
    extractQualified (ParenA _ qual) = extractQualified qual
    extractQualified other = error $ show other
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

eval_ :: [String] -> String -> IO (Either InterpreterError String)
eval_ modules line = runInterpreter $ do {
  setImports modules;
  eval line
}

runChecks :: Environment -> RType -> UProgram -> IO Bool
runChecks env goalType prog = runChecks'' modules funcSig body
  where
    args = _arguments env
    modules = "Prelude" : Set.toList (_included_modules env)
    argList = Map.toList args
    argNames = map fst argList
    argTypes = map snd argList
    monoGoals = map toMonotype argTypes
    funcSig = mkFunctionSigStr (monoGoals ++ [goalType])
    body = mkLambdaStr argNames prog

runChecks' :: [String] -> String -> String -> IO (Maybe String)
runChecks' modules funcSig body = if filterInf body then do
  arg <- (mapRandomArguments . parseTypeString) funcSig
  putStrLn (body ++ " " ++ arg)
  result <- eval_ modules (printf "((%s) :: %s) %s" body funcSig arg)
  case result of
    Left err -> putStrLn (displayException err) >> return Nothing
    Right res -> putStrLn res >> return (Just res)
else pure Nothing

runChecks'' :: [String] -> String -> String -> IO Bool
runChecks'' modules funcSig body = do
  result <- timeout 3000000 (runChecks' modules funcSig body)
  case result of
    Nothing -> return False
    Just Nothing -> return False
    Just (Just _) -> return True

filterInf :: String -> Bool
filterInf body = not $ any (`isInfixOf` body) excludeFunctions

-- not used due to the lack of specific types
-- https://www.oipapio.com/question-4824318
runIntegratedChecks :: [String] -> String -> String -> Int -> IO Bool
runIntegratedChecks modules funcSig body numTests = if filterInf body then
  let func = printf "((%s) :: %s)" body funcSig :: String
      prop = printf "((`seq` True) . %s)" func :: String
      modules' = "Test.QuickCheck":modules
  in do
    result <- runInterpreter $ do {
      setImports modules';
      act <- interpret ("verboseCheckResult " ++ prop) (as :: IO Result);

      liftIO act
    }
    case result of
      Left err -> putStrLn (displayException err) >> return False
      Right Success {} -> return True
      Right Failure {} -> return False
else pure False

checkDuplicate :: [String] -> String -> String -> String -> IO Bool
checkDuplicate modules funcSig bodyL bodyR = let fmt = printf "((%s) :: %s) %s" in
  if (not $ filterInf bodyL) || (not $ filterInf bodyR) then pure False
  else do
    arg <- (mapRandomArguments . parseTypeString) funcSig
    retL <- eval_ modules (fmt bodyL funcSig arg)
    retR <- eval_ modules (fmt bodyR funcSig arg)

    case (retL, retR) of
      (Right l, Right r) -> putStrLn "duplicate fcn" >> return (l /= r)
      _ -> return False
