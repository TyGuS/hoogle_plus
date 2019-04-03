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

import Data.Aeson
import Data.Maybe
import Text.Pretty.Simple
import System.Timeout
import System.Exit
import qualified Data.Map as Map

main :: IO ()
main = do
    tier1env <- newGenerateEnv genOptsTier1
    tier2env <- newGenerateEnv genOptsTier2
    queries <- readQueryFile queryFile
    let envs = [(tier1env, "Tier1"), (tier2env, "Tier2")]
    let params = [
          (searchParams, "Default"),
          (searchParamsBaseline, "Baseline"),
          (searchParamsZeroStart, "Zero Cover Start")]
    let exps = mkExperiments envs queries params
    resultSummaries <- runExperiments exps
    mapM_ showit resultSummaries
    where
      showit timing = pPrint timing


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
