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
import BPlot

import Data.Aeson
import Data.Yaml
import Data.Maybe
import Text.Pretty.Simple
import System.Console.CmdArgs.Implicit
import System.Timeout
import System.Exit
import System.FilePath.Posix
import Text.Printf
import qualified Data.Map as Map


defaultArgs = Args {
  argsQueryFile=defaultQueryFile &= name "queries" &= typFile,
  argsTimeout=defaultTimeout &= name "timeout" &= help "Each experiment will have N seconds to complete" ,
  argsOutputFile=[] &= name "output" &= typFile,
  argsExperiment=defaultExperiment &= argPos 0,
  argsOutputFormat=[Plot, TSV] &= name "format",
  argsPreset=ICFPPartial &= name "preset" &= help "Component set preset"
  }

main :: IO ()
main = do
    args <- cmdArgs defaultArgs
    let currentExperiment = argsExperiment args
    let setup = ExpSetup {expTimeout = argsTimeout args, expCourse = currentExperiment}
    (envs, params, exps) <- getSetup args
    let outputFormats = argsOutputFormat args
    let outputFormatFiles = zip outputFormats (repeat Nothing)
    let environmentStatsTable = outputSummary Table currentExperiment envs
    putStrLn environmentStatsTable

    resultSummaries <- runExperiments setup exps
    flip mapM_ (outputFormatFiles) (\(format, mbFile) -> do
      case format of
        Plot -> mkPlot mbFile setup resultSummaries
        _ -> do
          let resultTable = outputSummary format currentExperiment resultSummaries
          outputResults mbFile resultTable
      )

outputResults :: Maybe FilePath -> String -> IO ()
outputResults Nothing res = outputResults (Just "results.out") res
outputResults (Just fp) res = putStrLn res >> writeFile fp res

mkExperiments :: [(Environment, String)] -> [Query] -> [(SearchParams, String)] -> [Experiment]
mkExperiments envs qs params = [
  (env, envN, q, param, paramN) |
    (env, envN) <- envs,
    q <- qs,
    (param, paramN) <- params]

readQueryFile :: FilePath -> IO [Query]
readQueryFile fp = do
  let extension = takeExtension fp
  queries <- case extension of
                ".yaml" -> decodeYaml
                ".yml" -> decodeYaml
                ".json" -> decodeJson
                _ -> error "Unable to read query file. Must be .json or .yaml"
  printf "Number of queries: %d\n" (length queries)
  return queries
  where
    decodeJson = do
      mbQs <- decodeFileStrict' fp
      case mbQs of
        Nothing -> error "Unable to read query file. Is the JSON poorly formatted?"
        Just qs -> return qs
    decodeYaml = do
      eErrOrQs <- decodeFileEither fp
      case eErrOrQs of
        Left err ->  print err >> error "Unable to read query file. Is the YAML poorly formatted?"
        Right r -> return r

getSetup args = do
  let preset = argsPreset args
  componentSet <- generateEnv $ getOptsFromPreset preset
  componentOldSet <- generateEnv $ getOptsFromPreset ICFPPartial
  queries <- readQueryFile (argsQueryFile args)
  let envs = [(componentSet, show preset)]
  let currentExperiment = argsExperiment args
  let params =
        case currentExperiment of
          CoalescingStrategies -> [
            (searchParamsTyGarQ{_coalesceTypes=False}, expTyGarQNoCoalesce),
            (searchParamsTyGarQ{_coalesceTypes=True,
                                _coalesceStrategy=First},
              expTyGarQCoalesceFirst),
            (searchParamsTyGarQ{_coalesceTypes=True,
                                _coalesceStrategy=LeastInstantiated},
              expTyGarQCoalesceLeast),
            (searchParamsTyGarQ{_coalesceTypes=True,
                                _coalesceStrategy=MostInstantiated},
              expTyGarQCoalesceMost)
            ]
          CompareEnvironments -> let solnCount = 1  in
              [(searchParamsTyGarQ{_solutionCnt=solnCount}, expTyGarQ)]
          CompareSolutions -> let solnCount = 5 in
              [(searchParamsTyGarQ{_solutionCnt=solnCount}, expTyGarQ),
               (searchParamsSypetClone{_solutionCnt=solnCount}, expSypetClone)]
          CompareInitialAbstractCovers -> [
            (searchParamsTyGarQ, expTyGarQ),
            (searchParamsHOF, expQueryRefinementHOF),
            (searchParamsSypetClone, expSypetClone),
            (searchParamsTyGar0, expTyGar0),
            (searchParamsTyGarQB, expTyGarQB),
            (searchParamsTyGar0B, expTyGar0B),
            (searchParamsNoGar, expNoGar)]
          CompareFinalCovers -> [
            (searchParamsNoGar, expNoGar),
            (searchParamsNoGar0, expNoGar0)]
          CompareThresholds ->
            map (\i -> (searchParamsTyGar0 {_stopRefine=True,_threshold=i}, "TyGar0B"++show i)) [1..10]
            ++ map (\i -> (searchParamsTyGarQ {_stopRefine=True,_threshold=i}, "TyGarQB"++show i)) [1..10]
          CompareIncremental -> [
            (searchParamsNoGar, expNoGar),
            (searchParamsNoGarInc, expNoGarInc),
            (searchParamsTyGarQ, expTyGarQ),
            (searchParamsTyGarQInc, expTyGarQInc),
            (searchParamsTyGarQB, expTyGarQB),
            (searchParamsTyGarQBInc, expTyGarQBInc),
            (searchParamsTyGar0B, expTyGar0B),
            (searchParamsTyGar0BInc, expTyGar0BInc)]
          TrackTypesAndTransitions -> [
            (searchParamsTyGarQ{_coalesceTypes=True}, expTyGarQ),
            (searchParamsTyGarQ{_coalesceTypes=False}, expTyGarQNoCoalesce)]
  let exps =
        case currentExperiment of
          CompareEnvironments -> mkExperiments ((componentOldSet, show ICFPPartial):envs) queries params
          _ ->  mkExperiments envs queries params
  return (envs, params, exps)
