module Examples.ExampleChecker where

import Types.Program
import Types.Type
import Types.Environment
import Types.TypeChecker
import Synquid.Error
import Synquid.Parser

import Text.Parsec.Indent
import Text.Parsec.Pos
import Control.Monad.State
import GHC
import GHC.Paths
import Data.Typeable
import Outputable

type Example = [RProgram]

parseExample :: [String] -> IO ()
parseExample [] = return ()
parseExample (ex:exs) = do
    expr <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        timp <- parseImportDecl "import Data.Typeable"
        mimp <- parseImportDecl "import Data.Maybe"
        pimp <- parseImportDecl "import Prelude"
        setContext [IIDecl timp, IIDecl mimp, IIDecl pimp]
        exprType TM_Default ex
        -- result <- execStmt ("typeOf (" ++ ex ++ ")") execOptions
        -- case result of
        --  ExecComplete v _ -> case v of
        --                        Left _ -> return "error left"
        --                        Right n -> return "result right"
        --  ExecBreak b info -> return "error break"
        -- runDecls "a = 1"
        -- env <- getSession
        -- compileExprRemote "typeOf [1,2,3]"
    print $ showSDocUnsafe $ ppr expr
    -- print expr
    parseExample exs

checkExample :: Environment -> RType -> Example -> Either ErrorMessage Example
checkExample env typ@(ScalarT {}) [ex] = undefined
