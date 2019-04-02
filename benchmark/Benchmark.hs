module Main (main) where

import Types.Generate
import Types.Experiments
import Types.Environment
import Types.Program
import Synquid.Util
import HooglePlus.Synthesize
import Database.Environment
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
    queries <- readQueryFile queryFile
    let envs = [(tier1env, "Tier1")]
    let params = [(searchParams, "Default")]
    let exps = mkExperiments envs queries params
    resultSummaries <- mapM runExperiment exps
    mapM_ showit resultSummaries
    where
      showit (Nothing) = pPrint "Nothing"
      showit (Just timing) = pPrint timing

type Experiment = (Environment, String, Query, SearchParams, String)

mkExperiments :: [(Environment, String)] -> [Query] -> [(SearchParams, String)] -> [Experiment]
mkExperiments envs qs params = [
  (env, envN, q, param, paramN) |
    (env, envN) <- envs,
    q <- qs,
    (param, paramN) <- params]

runExperiment :: Experiment -> IO (Maybe ResultSummary)
runExperiment (env, envName, q, params, paramName) = do
    results <- runQuery env q params
    return ((summarizeResult envName paramName q) <$> results)

runQuery :: Environment -> Query -> SearchParams -> IO (Maybe [(RProgram, TimeStatistics)])
runQuery env q params = do
    let queryStr = query q
    goal <- envToGoal env queryStr
    timeout defaultTimeoutus $ synthesize params goal

readQueryFile :: FilePath -> IO [Query]
readQueryFile fp = do
  mbQs <- decodeFileStrict' fp
  case mbQs of
    Nothing -> undefined
    Just qs -> return qs

summarizeResult :: String -> String -> Query -> [(RProgram, TimeStatistics)] -> ResultSummary
summarizeResult envN paramN q results = ResultSummary {
    envName = envN,
    paramName = paramN,
    queryName = name q,
    queryStr = query q,
    solution = soln,
    tFirstSoln = totalTime firstR,
    tEncFirstSoln = encodingTime firstR,
    lenFirstSoln = pathLength firstR,
    refinementSteps = iterations firstR,
    transitions = safeTransitions
    }
    where
      safeTransitions = snd $ fromMaybe (0,0) $ listToMaybe $ (Map.toDescList (numOfTransitions firstR))
      soln = mkOneLine (show solnProg)
      (solnProg, firstR) =  errorhead "missing first solution" results
      errorhead msg xs = fromMaybe (error msg) $ listToMaybe xs

queryFile = "scripts/curated.json"
defaultTimeoutus = 100 * (10 ^ 6) -- 10 seconds in microseconds
searchParams = defaultSearchParams

genOptsTier1 = defaultGenerationOpts {
  modules = myModules,
  pkgFetchOpts = Local {
      files = ["libraries/tier1/base.txt", "libraries/tier1/bytestring.txt", "libraries/ghc-prim.txt"]
      }
  }

genOptsTier2 = genOptsTier1 {
  pkgFetchOpts = Local {
      files = ["libraries/base.txt", "libraries/bytestring.txt", "libraries/ghc-prim.txt"]
      }
  }

myModules = [
  -- base
  "Data.Int",
  "Data.Bool",
  "Data.Maybe",
  "Data.Tuple",
  "GHC.Char",
  "Text.Show",
  -- ByteString
  "Data.ByteString.Lazy",
  "Data.ByteString.Builder"
  ]
{-
get benchmarks from file
tier 1
tier 2
HOF for each, that is a synthesis flag

library files -> environment
query file -> [query]
environment -> query -> params -> Stream Results

-}
