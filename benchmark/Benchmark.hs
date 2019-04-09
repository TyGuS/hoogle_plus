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
import BTypes
import BOutput

import Data.Aeson
import Data.Maybe
import Text.Pretty.Simple
import System.Timeout
import System.Exit
import qualified Data.Map as Map

main :: IO ()
main = do
    tier1env <- generateEnv genOptsTier1
    tier2env <- generateEnv genOptsTier2
    queries <- readQueryFile queryFile
    let envs = [(tier1env, "Total")]
    let params = [
          (searchParams, expQueryRefinement),
          (searchParamsHOF, expQueryRefinementHOF),
          (searchParamsBaseline, expBaseline),
          (searchParamsZeroStart, expZeroCoverStart)]
    let baseExps = mkExperiments envs queries params
    let extraExps = mkExperiments [(tier2env, "Partial")] queries [(searchParamsHOF, expQueryRefinementHOF)]
    let exps = baseExps ++ extraExps
    resultSummaries <- runExperiments exps
    let aggregatedResults = toGroup resultSummaries
    let table = toTable aggregatedResults
    putStrLn table
    -- pPrint aggregatedResults


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
    Nothing -> undefined
    Just qs -> return qs
