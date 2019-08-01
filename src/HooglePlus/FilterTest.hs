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
                    deriving (Show, Eq)

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

mapRandomArguments :: (TypeEnv, [ArgumentType], ArgumentType) -> IO String
mapRandomArguments (typeEnv, args, retType)  = unwords <$> mapM f (transformType typeEnv args)
  where
    f (Concrete x) = generateRandomValue x
    f (Polymorphic x) = generateRandomValue "Int"
    -- ! fixme - extend to any type

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

runChecks :: Environment -> RType -> UProgram -> IO Bool
runChecks env goalType prog = runChecks'' modules funcSig body
  where
    args = _arguments env
    modules = "Prelude" : Set.toList (_included_modules env)
    argList = Map.toList args
    argNames = map fst argList
    argTypes = map snd argList
    monoGoals = (map toMonotype argTypes)
    funcSig = mkFunctionSigStr (monoGoals ++ [goalType])
    body = mkLambdaStr argNames prog


runChecks' :: [String] -> String -> String -> IO Bool
runChecks' modules funcSig body = if preCheck body then do
  arg <- (mapRandomArguments . parseTypeString) funcSig
  putStrLn (body ++ " " ++ arg)
  result <- runInterpreter $ do {
    setImports modules;
    eval ("((" ++ body ++ ") :: " ++ funcSig ++ ") " ++ arg)
  }
  case result of
    Left err -> putStrLn (displayException err) >> return False
    Right res -> putStrLn res >> return True
else pure False

runChecks'' :: [String] -> String -> String -> IO Bool
runChecks'' modules funcSig body = do
  result <- timeout 3000000 (runChecks' modules funcSig body)
  case result of
    Nothing -> return False
    Just val -> return val

preCheck :: String -> Bool
preCheck body = not $ any (`isInfixOf` body) excludeFunctions