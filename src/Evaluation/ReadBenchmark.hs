module Evaluation.ReadBenchmark where

import Data.Yaml
import System.IO

import Evaluation.Benchmark

readSuite :: FilePath -> IO [Benchmark]
readSuite fp = do
    putStrLn $ "Reading benchmarks from " ++ fp ++ "..."
    decodeResult <- decodeFileEither fp
    case decodeResult of
        Left err -> error $ prettyPrintParseException err 
        Right v -> return v
