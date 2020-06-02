{-# LANGUAGE DeriveGeneric #-}
module Evaluation.Benchmark where

import GHC.Generics
import Data.Aeson (genericParseJSON, defaultOptions)
import Data.Yaml

data Benchmark = Benchmark {
    name :: String,
    query :: String,
    solution :: String,
    source :: String
} deriving(Eq, Generic, Show)

instance FromJSON Benchmark where
