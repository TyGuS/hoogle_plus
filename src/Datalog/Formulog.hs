module Datalog.Formulog
    ( runFormulog
    , writeFormulog
    ) where

import Database.Environment
import Datalog.Datalog
import Datalog.DatalogType
import Datalog.FormulogType
import Datalog.Utils
import Types.Common
import Types.Environment
import Types.Type
import Types.Experiments
import Types.Program
import Types.IOFormat
import HooglePlus.Utils
import Synquid.Type

import Control.Lens
import Control.Monad.Logic
import Data.List
import qualified Data.Map as Map
import System.IO
import System.Directory
import System.Process
import Text.Read
import Text.Printf

formulogPreamble = unlines [ "type tvar = string"
                           , "type var = string"
                           , "type typ = typ_tvar(tvar) | typ_datatype(tvar, typ list)"
                           , "type exp = exp_var(var) | exp_app(var, exp list)"
                           , "fun append(L1 : 'a list, L2 : 'a list) : 'a list ="
                           , "match L1 with"
                           , "| [] => L2"
                           , "| X :: L1rest => X :: append(L1rest, L2)"
                           , "end"
                           , "fun elem(X : 'a, L : 'a list) : bool ="
                           , "match L with"
                           , "| [] => false"
                           , "| Y :: Lrest => X = Y || elem(X, Lrest)"
                           , "end"
                           , "input inh(typ, var)"
                           , "output sat(typ, exp, typ list, i32)"
                           , "output result(exp)"
                           ]

runFormulog :: SearchParams -> Environment -> SchemaSkeleton -> [Example] -> Int -> LogicT IO ()
runFormulog params env goal examples d = do
    paths <- liftIO $ findPath env goal d
    ifte (msum $ map (enumeratePath params env goal examples) paths)
         return
         (runFormulog params env goal examples (d + 1))

findPath :: Environment -> SchemaSkeleton -> Int -> IO [UProgram]
findPath env goal d = do
    -- get higher-order arguments
    let args = map (over _2 toMonotype) (Map.toList (env ^. arguments))
    let hoArgs = filter (isFunctionType . snd) args
    let hoArgSat = map (uncurry writeFunctionFormulog) hoArgs
    -- write query into the file
    let dst = varToDatatype (lastType (toMonotype goal))
    let query = printf "result(P) :- sat(%s, P, D), D = %d." (writeType (FormulogPack dst)) d
    -- write depth into the constraints
    let src = "./data/formulog/input.flg"
    let dst = "./data/formulog/main.flg"
    fileContent <- readFile src
    -- write the arguments into the file
    let packedArgs = map (over _2 FormulogPack) args
    let argsStr = map (uncurry writeArg) packedArgs
    writeFile dst (replaceId "{}" (show (d - 1)) fileContent ++ unlines (query : hoArgSat ++ argsStr))
    
    -- execute the solver
    out <- readProcess "java" ["-DprintResults=\"some:result\"", "-jar", "./data/formulog/formulog-0.3.0.jar", "./data/formulog/main.dl"] ""
    -- read results
    return $ map read $ lines out :: IO [UProgram]

writeFormulog :: Environment -> IO ()
writeFormulog env =
    -- write datalog templates
    writeFile "./data/formulog/input.flg" $
        unlines (formulogPreamble : map (uncurry writeFunctionFormulog) (Map.toList $ env ^. groups))

writeFunctionFormulog :: Id -> TypeSkeleton -> String
writeFunctionFormulog = writeFunction "sat(%s, exp_app(\"%s\", %s), %s)" (foldr (printf "%s :: %s") "[]") FormulogPack

