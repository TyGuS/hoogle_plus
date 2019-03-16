module Main (main) where

import Types.Generate
import Types.Experiments
import HooglePlus.Synthesize
import Database.Environment

import Data.Aeson


main :: IO ()
main = do
  env <- newGenerateEnv genOpts
  let goal = mkGoal env query
  synthesize searchParams goal
  return ()


genOpts = defaultGenerationOpts {
  modules = [
      "Data.Maybe",
      "Data.ByteString.Lazy"
      ],
  pkgFetchOpts = Local {
      files = ["libraries/tier1/base.txt", "libraries/tier1/bytestring.txt"]
      }
  }

{-
get benchmarks from file
tier 1
tier 2
HOF for each, that is a synthesis flag

library files -> environment
query file -> [query]
environment -> query -> params -> Stream Results

-}
