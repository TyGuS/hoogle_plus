module HooglePlus.GHCChecker (haskellTypeChecks) where

import Language.Haskell.Interpreter

import qualified Data.Set as Set hiding (map)
import Data.Map as Map hiding (map, foldr)
import Control.Exception

import Synquid.Program
import Synquid.Type
import Synquid.Pretty as Pretty

say :: String -> Interpreter ()
say = liftIO . putStrLn


haskellTypeChecks :: Environment -> RType -> UProgram -> IO Bool
haskellTypeChecks env goalType prog = let
    args = _arguments env
    modules = Set.toList $ _included_modules env
    argList = Map.toList args
    argNames = map fst argList
    argTypes = map snd argList
    funcSig = mkFunctionSigStr (map toMonotype argTypes) goalType
    body = mkLambdaStr argNames prog
    expr = body ++ " :: " ++ funcSig
    hintQuery :: Interpreter Bool
    hintQuery = do
        setImports ("Prelude":modules)
        say $ "importing: " ++ unlines modules
        say $ "Checking:"
        say $ expr
        -- Ensures that if there's a problem we'll know
        Language.Haskell.Interpreter.typeOf expr
        typeChecks expr
    in do
        r <- runInterpreter hintQuery
        case r of
            Left err -> (putStrLn $ displayException err) >> return False
            Right False -> (putStrLn "Program does not typecheck") >> return False
            Right True -> (putStrLn "Program typechecks according to Haskell!") >> return True

-- mkFunctionSigStr generates a function's type signature:
-- Int -> Data.Foo.Foo -> Bar
mkFunctionSigStr :: [RType] -> RType -> String
mkFunctionSigStr [] tyRet = show tyRet
mkFunctionSigStr (argTy:argTys) tyRet
    = show argTy ++ " -> " ++ mkFunctionSigStr argTys tyRet

-- mkLambdaStr produces a oneline lambda expr str:
-- (\x -> \y -> body))
mkLambdaStr :: [String] -> UProgram -> String
mkLambdaStr args body = let
    bodyStr = show body
    oneLineBody = unwords $ lines bodyStr
    addFuncArg arg rest = Pretty.parens $ text ("\\" ++ arg ++ " -> ") <+> rest
    in
        show $ foldr addFuncArg (text oneLineBody) args
