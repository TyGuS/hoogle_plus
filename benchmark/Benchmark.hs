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
import Data.Yaml
import Data.Maybe
import Text.Pretty.Simple
import System.Console.CmdArgs.Implicit
import System.Timeout
import System.Exit
import System.FilePath.Posix
import qualified Data.Map as Map


defaultArgs = Args {
  argsQueryFile=defaultQueryFile &= name "queries" &= typFile,
  argsTimeout=defaultTimeout &= name "timeout" &= help "Each experiment will have N seconds to complete" ,
  argsOutputFile=Nothing &= name "output" &= typFile,
  argsExperiment=defaultExperiment &= argPos 0,
  argsOutputFormat=TSV &= name "format",
  argsPreset=POPL &= name "preset" &= help "Component set preset"
  }

main :: IO ()
main = do
    args <- cmdArgs defaultArgs
    let currentExperiment = argsExperiment args
    let setup = ExpSetup {expTimeout = argsTimeout args, expCourse = currentExperiment}
    (envs, params, exps) <- getSetup args
    let outputFormat = argsOutputFormat args
    let environmentStatsTable = outputSummary Table currentExperiment envs
    putStrLn environmentStatsTable

    resultSummaries <- runExperiments setup exps
    let resultTable = outputSummary outputFormat currentExperiment resultSummaries
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
  let extension = takeExtension fp
  case extension of
    ".yaml" -> decodeYaml
    ".yml" -> decodeYaml
    ".json" -> decodeJson
    _ -> error "Unable to read query file. Must be .json or .yaml"
  where
    decodeJson = do
      mbQs <- decodeFileStrict' fp
      case mbQs of
        Nothing -> error "Unable to read query file. Is the JSON poorly formatted?"
        Just qs -> return qs
    decodeYaml = do
      eErrOrQs <- decodeFileEither fp
      case eErrOrQs of
        Left _ ->  error "Unable to read query file. Is the YAML poorly formatted?"
        Right r -> do
          print r
          return r

getSetup args = do
  let preset = argsPreset args
  componentSet <- generateEnv $ getOptsFromPreset preset
  componentOldSet <- generateEnv $ getOptsFromPreset ICFPPartial
  queries <- readQueryFile (argsQueryFile args)
  let envs = [(componentSet, show preset), (componentOldSet, show ICFPPartial)]
  let currentExperiment = argsExperiment args
  let params =
        case currentExperiment of
          CompareEnvironments -> let solnCount = 5 in
              [(searchParamsTyGarQ{_solutionCnt=solnCount}, expTyGarQ)]
          CompareSolutions -> let solnCount = 5 in
              [(searchParamsTyGarQ{_solutionCnt=solnCount}, expTyGarQ)]
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
  let exps = mkExperiments envs queries params
  return (envs, params, exps)
