{-# LANGUAGE DeriveGeneric #-}
module Evaluation.Benchmark where

import GHC.Generics
import Data.Aeson (genericParseJSON, defaultOptions)
import Data.Yaml
import Types.IOFormat (Example)

data Benchmark = Benchmark {
    name :: String,
    query :: String,
    solution :: String,
    source :: String,
    examples :: [Example]
} deriving(Eq, Generic, Show)

instance FromJSON Benchmark where
