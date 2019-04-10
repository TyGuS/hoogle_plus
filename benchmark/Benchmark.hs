module Main (main) where

import Types.Generate
import Types.Experiments
import Types.Environment
import Types.Program
import Synquid.Util
import HooglePlus.Synthesize
import Database.Environment
import Runner
import BConfig
import BTypes hiding (name)
import BOutput

import Data.Aeson
import Data.Maybe
import Text.Pretty.Simple
import System.Console.CmdArgs.Implicit
import System.Timeout
import System.Exit
import qualified Data.Map as Map


defaultArgs = Args {
  argsQueryFile=defaultQueryFile &= name "queries" &= typFile,
  argsTimeout=defaultTimeout &= name "timeout" &= help "Each experiment will have N seconds to complete" ,
  argsOutputFile=Nothing &= name "output" &= typFile
  }
  where
    defaultQueryFile = "benchmark/suites/allQueries.json"
    defaultTimeout = 5 * 60 :: Int


main :: IO ()
main = do
    args <- cmdArgs defaultArgs
    tier1env <- generateEnv genOptsTier1
    tier2env <- generateEnv genOptsTier2
    queries <- readQueryFile (argsQueryFile args)
    let envs = [(tier1env, "Total")]
    let params = [
          (searchParams, expQueryRefinement),
          (searchParamsHOF, expQueryRefinementHOF),
          (searchParamsBaseline, expBaseline),
          (searchParamsZeroStart, expZeroCoverStart)]
    let baseExps = mkExperiments envs queries params
    let extraExps = mkExperiments [(tier2env, "Partial")] queries [(searchParamsHOF, expQueryRefinementHOF)]
    let exps = baseExps ++ extraExps
    let setup = ExpSetup {expTimeout = argsTimeout args}
    resultSummaries <- runExperiments setup exps
    let aggregatedResults = toGroup resultSummaries
    let table = toTable aggregatedResults
    outputResults (argsOutputFile args) table

outputResults :: Maybe FilePath -> String -> IO ()
outputResults Nothing res = putStrLn res
outputResults (Just fp) res = putStrLn res >> writeFile fp res

mkExperiments :: [(Environment, String)] -> [Query] -> [(SearchParams, String)] -> [Experiment]
mkExperiments envs qs params = [
  (env, envN, q, param, paramN) |
    (env, envN) <- envs,
    q <- qs,
    (param, paramN) <- params]

readQueryFile :: FilePath -> IO [Query]
readQueryFile fp = do
  mbQs <- decodeFileStrict' fp
  case mbQs of
    Nothing -> error "Unable to read query file. Is the JSON poorly formatted?"
    Just qs -> return qs
