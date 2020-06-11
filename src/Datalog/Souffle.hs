module Datalog.Souffle (
    runSouffle
    ) where

import Datalog.Datalog
import Datalog.SouffleType
import Database.Environment
import Text.Read
import Types.Experiments
import Types.Environment
import Types.Type
import Types.IOFormat
import Types.Program
import HooglePlus.Utils
import Synquid.Type

import Control.Monad.Logic
import Control.Lens
import qualified Data.Map as Map
import System.Process
import System.IO
import System.Directory

runSouffle :: SearchParams -> Environment -> RSchema -> [Example] -> Int -> LogicT IO ()
runSouffle params env goal examples d = do
    paths <- liftIO $ findPath env d
    ifte (msum $ map (enumeratePath params env goal examples) paths)
         return
         (runSouffle params env goal examples (d + 1))

findPath :: Environment -> Int -> IO [UProgram]
findPath env d = do
    -- get higher-order arguments
    let args = Map.toList (env ^. arguments)
    let hoArgs = filter (isFunctionType . toMonotype . snd) args
    let hoArgSat = map (uncurry writeFunction) hoArgs
    -- write depth into the constraints
    let src = "./data/souffle/input.dl"
    let dst = "./data/souffle/main.dl"
    fileContent <- readFile src
    writeFile dst (replaceId "{}" (show d) fileContent ++ unlines hoArgSat)
    -- write the arguments into the file
    writeFile "./data/souffle/inh.facts" (unlines $ map (uncurry writeArg) args)
    -- execute the solver
    readProcess "souffle" ["--fact-dir=./data/souffle/", "--output-dir=./data/souffle/", "./data/souffle/main.dl"] ""
    -- read results
    out <- readFile "./data/souffle/query.csv"
    return $ map read $ lines out :: IO [UProgram]
