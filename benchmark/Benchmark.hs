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
          CompareSolutions -> let solnCount = 5 in
              [ (searchParamsTyGarQ{_solutionCnt=solnCount}, expTyGarQ),
                (searchParamsTyGarQ{_disableDemand=True, _solutionCnt=solnCount}, expTyGarQNoDmd)]
          CompareInitialAbstractCovers -> [
            (searchParamsTyGarQ, expTyGarQ),
            -- (searchParamsHOF, expQueryRefinementHOF),
            (searchParamsSypetClone, expSypetClone),
            (searchParamsTyGar0, expTyGar0),
            (searchParamsTyGarQB, expTyGarQB),
            (searchParamsTyGar0B, expTyGar0B),
            (searchParamsNoGar, expNoGar)]
          CompareFinalCovers -> [
            (searchParamsNoGar, expNoGar),
            (searchParamsNoGar0, expNoGar0),
            (searchParamsNoGarTyGar0, expNoGarTyGar0),
            (searchParamsNoGarTyGarQ, expNoGarTyGarQ),
            (searchParamsNoGarTyGar0B, expNoGarTyGar0B),
            (searchParamsNoGarTyGarQB, expNoGarTyGarQB)]
          CompareThresholds ->
            map (\i -> (searchParamsTyGar0 {_stopRefine=True,_threshold=i}, "TyGar0B"++show i)) [1..10]
            ++ map (\i -> (searchParamsTyGarQ {_stopRefine=True,_threshold=i}, "TyGarQB"++show i)) [1..10]
          TrackTypesAndTransitions -> [
            (searchParamsTyGarQ{_coalesceTypes=True}, expTyGarQ),
            (searchParamsTyGarQ{_coalesceTypes=False}, expTyGarQNoCoalesce)]
  let exps =
        case currentExperiment of
          TrackTypesAndTransitions -> mkExperiments envs queries params
          CompareSolutions -> mkExperiments envs queries params
          _ -> let
            baseExps = mkExperiments envs queries params
            in baseExps -- ++ extraExps
  return (envs, params, exps)
