module Main (main) where

import Types.Generate
import Types.Experiments
import Types.Environment
import Types.Program
import Synquid.Util
import HooglePlus.Synthesize
import Database.Presets
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
  argsOutputFile=Nothing &= name "output" &= typFile,
  argsExperiment=defaultExperiment &= name "experiment",
  argsOutputFormat=Table &= name "format"
  }
  where

main :: IO ()
main = do
    args <- cmdArgs defaultArgs
    let currentExperiment = argsExperiment args
    let setup = ExpSetup {expTimeout = argsTimeout args, expCourse = currentExperiment}
    (envs, params, exps) <- getSetup args
    resultSummaries <- runExperiments setup exps
    let outputFormat = argsOutputFormat args
    let environmentStatsTable = outputSummary Table currentExperiment envs
    let resultTable = outputSummary outputFormat currentExperiment resultSummaries
    putStrLn environmentStatsTable
    outputResults (argsOutputFile args) (resultTable)

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

getSetup args = do
  tier1env <- generateEnv genOptsTier1
  tier2env <- generateEnv genOptsTier2
  queries <- readQueryFile (argsQueryFile args)
  let envs = [(tier2env, "Partial")]
  let currentExperiment = argsExperiment args
  let params =
        case currentExperiment of
          CompareInitialAbstractCovers -> [
            (searchParams, expQueryRefinement),
            (searchParamsHOF, expQueryRefinementHOF),
            (searchParamsBaseline, expBaseline),
            (searchParamsZeroStart, expZeroCoverStart)]
          TrackTypesAndTransitions -> [(searchParams, expQueryRefinement)]
          CompareSolutions -> let solnCount = 5 in
              [ (searchParams{_solutionCnt=solnCount}, expQueryRefinement),
                (searchParams{_disableDemand=True, _solutionCnt=solnCount}, expQueryRefinementNoDemand)]
  let exps =
        case currentExperiment of
          CompareInitialAbstractCovers -> let
            baseExps = mkExperiments envs queries params
            extraExps = mkExperiments [(tier2env, "Partial")] queries [(searchParamsHOF, expQueryRefinementHOF)]
            in baseExps ++ extraExps
          TrackTypesAndTransitions -> mkExperiments envs queries params
          CompareSolutions -> mkExperiments envs queries params
  return (envs, params, exps)
