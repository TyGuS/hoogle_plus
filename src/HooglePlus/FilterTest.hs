module HooglePlus.FilterTest (runGhcChecks, getTypeString, runNoExceptionTest, runChecks) where

import Language.Haskell.Interpreter
import Language.Haskell.Exts.Parser
import Text.Printf
import Control.Exception
import Data.List
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty

import Test.QuickCheck

import HooglePlus.Utils
import Types.Environment
import Types.Program
import Types.Type hiding (typeOf)
import Synquid.Type

import qualified Data.Map as Map hiding (map, foldr)
import qualified Data.Set as Set hiding (map)

data ArgumentType = Concrete String
                  | Polymorphic String
                  | ArgTypeList ArgumentType
                    deriving (Show, Eq)

type TypeEnv = [(ArgumentType, String)]

getTypeString :: [String] -> String -> IO (Either InterpreterError String)
getTypeString imports input =
  runInterpreter $ do {
    setImports imports;
    Language.Haskell.Interpreter.typeOf input
  }

parseTypeString :: String -> (TypeEnv, [ArgumentType], ArgumentType)
parseTypeString input = buildRet [] [] value
  where
    (ParseOk value) = parseType input

    argName (TyVar _ (Ident _ name)) = Polymorphic name
    argName (TyCon _ (UnQual _ (Ident _ name))) = Concrete name
    argName (TyList _ arg) = ArgTypeList (argName arg)

    buildRet typeEnv argList (TyForall _ _ (Just ctx) t) = buildRet typeEnv' argList t
      where typeEnv' = typeEnv ++ buildEnv typeEnv ctx
    buildRet typeEnv argList (TyFun _ typeArg typeRet) = buildRet typeEnv argList' typeRet
      where argList' = argList ++ [argName typeArg]
    buildRet typeEnv argList typeRet = (typeEnv, argList, argName typeRet)

    extractQualified (ClassA _ (UnQual _ (Ident _ name)) var) =
      map (\x -> (argName x, name)) var
    buildEnv typeEnv (CxSingle _ item) = typeEnv ++ extractQualified item
    buildEnv typeEnv (CxTuple _ list) = foldr ((++) . extractQualified) typeEnv list


-- todo: implement
runGhcChecks :: String -> IO Bool
runGhcChecks input = return False

generatePrimitiveTestData :: String -> IO String
generatePrimitiveTestData pType =
  let pre str = "(" ++ str ++ ")" in case pType of
  "Int" ->
    do result <- generate arbitrary :: IO Int
       return $ pre $ show result
  "Bool" ->
    do result <- generate arbitrary :: IO Bool
       return $ pre $ show result
  "String" ->
    do result <- generate arbitrary :: IO String
       return $ pre $ format $ show result
    where
      format str = prettyPrint (String [] str "")

prepareArguments :: [String] -> String -> IO String
prepareArguments imports input =
  do
    (Right typeStr) <- getTypeString imports input
    unwords <$> mapM f (transformTypePre $ parseTypeString typeStr)
  where
    f (Concrete x) = generatePrimitiveTestData x
    f (Polymorphic x) = generatePrimitiveTestData "Int"

    transformTypePre (typeEnv, argsType, _) =
      transformType typeEnv argsType

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

prepareArguments' :: (TypeEnv, [ArgumentType], ArgumentType) -> IO String
prepareArguments' result =
  do
    unwords <$> mapM f (transformTypePre result)
  where
    f (Concrete x) = generatePrimitiveTestData x
    f (Polymorphic x) = generatePrimitiveTestData "Int"

    transformTypePre (typeEnv, argsType, _) =
      transformType typeEnv argsType

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

runNoExceptionTest :: String -> IO Bool
runNoExceptionTest input =
  let
    importList = ["Prelude", "Data.List", "Data.Maybe"]
    code = "(" ++ input ++ ")"
  in do
    arg <- prepareArguments importList input
    result <- runInterpreter $ do {
      setImports importList;
      eval (code ++ " " ++ arg)
    }
    case result of
      Left err -> putStrLn (displayException err) >> return False
      Right res -> putStrLn res >> return True

-- https://github.com/haskell-hint/hint/issues/77
test :: IO (Either InterpreterError String)
test = runInterpreter $ do {
  setImports ["Prelude", "Test.QuickCheck"];
  act <- interpret "generate arbitrary :: IO String" (as :: IO String);
  liftIO act
}

runChecks :: Environment -> RType -> UProgram -> IO Bool
runChecks env goalType prog = let
  args = _arguments env
  modules = Set.toList $ _included_modules env
  argList = Map.toList args
  argNames = map fst argList
  argTypes = map snd argList
  monoGoals = (map toMonotype argTypes)
  funcSig = mkFunctionSigStr (monoGoals ++ [goalType])
  body = mkLambdaStr argNames prog


  in do

    arg <- prepareArguments' $ parseTypeString funcSig
    result <- runInterpreter $ do {
      setImports modules;
      eval (body ++ " " ++ arg)
    }
    case result of
      Left err -> putStrLn (displayException err) >> return False
      Right res -> putStrLn res >> return True
    -- call test procedures
    return False