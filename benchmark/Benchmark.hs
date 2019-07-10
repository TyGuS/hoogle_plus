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
  argsOutputFormat=[] &= name "format",
  argsPreset=ICFPPartial &= name "preset" &= help "Component set preset"
  }

main :: IO ()
main = do
    args <- cmdArgs defaultArgs
    let currentExperiment = argsExperiment args
    let setup = ExpSetup {expTimeout = argsTimeout args, expCourse = currentExperiment}
    (envs, params, exps) <- getSetup args
    let outputFormats' = argsOutputFormat args
    let outputFormats = if null outputFormats' then [Plot, TSV] else outputFormats'
    let outputFiles = argsOutputFile args
    let outputFormatFiles = zip outputFormats ((map Just outputFiles) ++ repeat Nothing)
    let environmentStatsTable = outputSummary Table currentExperiment envs
    putStrLn environmentStatsTable

    resultSummaries <- runExperiments setup exps
    flip mapM_ outputFormatFiles (\(format, mbFile) -> do
      case format of
        None -> return ()
        Plot -> mkPlot mbFile setup resultSummaries
        _ -> do
          let resultTable = outputSummary format currentExperiment resultSummaries
          outputResults format mbFile resultTable
      )

outputResults :: ResultFormat -> Maybe FilePath -> String -> IO ()
outputResults format Nothing res = outputResults format (Just (defaultFormatFilename format)) res
outputResults _ (Just fp) res = putStrLn res >> writeFile fp res

defaultFormatFilename :: ResultFormat -> FilePath
defaultFormatFilename TSV = "results.tsv"
defaultFormatFilename Latex = "results.tex"
defaultFormatFilename Table = "results.txt"
defaultFormatFilename Plot = "results.plot"

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
          POPLQuality -> let solnCount = 5 in [
            (searchParamsTyGarQ{_useHO=True, _solutionCnt=solnCount}, expTyGarQ),
            (searchParamsTyGarQNoDmd{_useHO=True, _solutionCnt=solnCount}, expTyGarQNoDmd),
            (searchParamsTyGarQNoRel{_useHO=True, _solutionCnt=solnCount}, expTyGarQNoRel)
            ]
          POPLSpeed -> [
            (searchParamsTyGarQ, expTyGarQ),
            (searchParamsTyGarQB, expTyGarQB ++ "-5"),
            (searchParamsTyGar0, expTyGar0),
            (searchParamsTyGar0B, expTyGar0B ++ "-5"),
            (searchParamsNoGar, expNoGar),
            (searchParamsSypetClone, expSypetClone)
            ]
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
          CompareCopyTransitions -> [
            (searchParamsTyGarQ{_disableCopy=False, _disableFS=False}, expTyGarQ ++ "both")   ,
            (searchParamsTyGarQ{_disableCopy=True, _disableFS=False} , expTyGarQ ++ "no copy"),
            (searchParamsTyGarQ{_disableCopy=False, _disableFS=True} , expTyGarQ ++ "no fs")  ,
            (searchParamsTyGarQ{_disableCopy=True, _disableFS=True}  , expTyGarQ ++ "neither"),
            (searchParamsTyGarQB{_disableCopy=False, _disableFS=False}, expTyGarQB ++ "both"),
            (searchParamsTyGarQB{_disableCopy=True, _disableFS=False}, expTyGarQB ++ "no copy"),
            (searchParamsTyGarQB{_disableCopy=False, _disableFS=True}, expTyGarQB ++ "no fs"),
            (searchParamsTyGarQB{_disableCopy=True, _disableFS=True}, expTyGarQB ++ "neither"),
            (searchParamsTyGar0{_disableCopy=False, _disableFS=False}, exprTyGar0 ++ "both"),
            (searchParamsTyGar0{_disableCopy=True, _disableFS=False}, exprTyGar0 ++ "no copy"),
            (searchParamsTyGar0{_disableCopy=False, _disableFS=True}, exprTyGar0 ++ "no fs"),
            (searchParamsTyGar0{_disableCopy=True, _disableFS=True}, exprTyGar0 ++ "neither"),
            (searchParamsNoGar{_disableCopy=False, _disableFS=False}, expNoGar ++ "both"),
            (searchParamsNoGar{_disableCopy=True, _disableFS=False}, expNoGar ++ "no copy"),
            (searchParamsNoGar{_disableCopy=False, _disableFS=True}, expNoGar ++ "no fs"),
            (searchParamsNoGar{_disableCopy=True, _disableFS=True}, expNoGar ++ "neither")
              )
            ]
          CompareEnvironments -> let solnCount = 1  in
              [(searchParamsTyGarQ{_solutionCnt=solnCount}, expTyGarQ)]
          CompareSolutions -> let solnCount = 5 in
              [(searchParamsTyGarQ{_solutionCnt=solnCount}, expTyGarQ),
               (searchParamsSypetClone{_solutionCnt=solnCount}, expSypetClone)]
          CompareInitialAbstractCovers -> [
            (searchParamsTyGarQ, expTyGarQ),
            (searchParamsSypetClone, expSypetClone),
            (searchParamsTyGar0, expTyGar0)
            -- (searchParamsTyGarQB, expTyGarQB),
            -- (searchParamsTyGar0B, expTyGar0B),
            -- (searchParamsNoGar, expNoGar)
            ]
          CompareFinalCovers -> [
            (searchParamsNoGar, expNoGar),
            (searchParamsNoGar0, expNoGar0)]
          CompareThresholds -> let
            bounds = 1:[5,10..25]
            in
            map (\i -> (searchParamsTyGar0 {_stopRefine=True,_threshold=i}, "TyGar0B"++show i)) bounds
            ++ map (\i -> (searchParamsTyGarQ {_stopRefine=True,_threshold=i}, "TyGarQB"++show i)) bounds
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
