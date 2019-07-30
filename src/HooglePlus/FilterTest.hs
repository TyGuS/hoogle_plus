module HooglePlus.FilterTest (runGhcChecks, getTypeString, runNoExceptionTest) where

import Language.Haskell.Interpreter
import Language.Haskell.Exts.Parser
import Text.Printf
import Control.Exception
import Data.List
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty

import Test.QuickCheck

data ArgumentType = Instance String | Polymorphic String deriving (Show)

getTypeString :: String -> IO (Either InterpreterError String)
getTypeString input =
  runInterpreter $ do {
    setImports ["Prelude", "Data.List", "Data.Maybe"];
    typeOf input
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

-- todo: support for list/custom data types
prepareArguments :: String-> IO String
prepareArguments input =
  do
    (Right typeStr) <- getTypeString input
    unwords <$> mapM f (fst $ parseTypeString typeStr)
  where
    f (Instance x) = generatePrimitiveTestData x
    f (Polymorphic x) = generatePrimitiveTestData "Int"

runNoExceptionTest :: String -> IO Bool
runNoExceptionTest input =
  let
    importList = ["Prelude", "Data.List", "Data.Maybe"]
    code = "(" ++ input ++ ")"
  in do
    arg <- prepareArguments input
    result <- runInterpreter $ do {
      setImports importList;
      eval (code ++ " " ++ arg)
    }
    case result of
      Left err -> putStrLn (displayException err) >> return False
      Right res -> putStrLn res >> return True

