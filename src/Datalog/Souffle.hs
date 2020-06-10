module Datalog.Souffle (
    runSouffle
    ) where

import Datalog.Datalog
import Text.Read
import Types.Experiment
import Types.Environment
import Types.Type
import Types.IOFormat

import System.Process
import System.IO
import System.Directory

runSouffle :: SearchParams -> Environment -> RSchema -> [Example] -> Int -> IO ()
runSouffle params env goal examples d = do
    paths <- findPath env d
    ite (msum $ map (enumeratePath params env goal examples) paths)
        (return ())
        (runSouffle params env goal examples (d + 1))

findPath :: Environment -> Int -> IO [UProgram]
findPath env d = do
    -- get higher-order arguments
    let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let hoArgSat = map (uncurry writeFunction) hoArgs
    -- write depth into the constraints
    let source = "./souffle/main.dl"
    fileContent <- readFile source
    writeFile source (replaceId "{}" (show d) fileContent ++ unlines hoArgSat)
    -- write the arguments into the file
    writeFile ("./souffle/inh.facts") (unlines $ map (uncurry writeArg) (env ^. arguments))
    -- execute the solver
    readProcess "souffle" ["--facts=./souffle/", "--output=./souffle/"] ""
    -- read results
    out <- readFile "./souffle/query.csv"
    return $ map fst (readList $ replaceId "nil" "[]" out) :: [SProgram]
