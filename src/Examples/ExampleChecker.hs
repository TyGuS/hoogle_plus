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
import Outputable

type Example = [RProgram]

parseExample :: [String] -> IO ()
parseExample [] = return ()
parseExample (ex:exs) = do
    print libdir
    ty <- runGhc (Just libdir) (exprType TM_Inst ex)
    print (showSDocUnsafe $ pprParendType ty)
    parseExample exs

checkExample :: Environment -> RType -> Example -> Either ErrorMessage Example
checkExample env typ@(ScalarT {}) [ex] = undefined
