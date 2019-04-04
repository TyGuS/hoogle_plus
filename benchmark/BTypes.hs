{-# LANGUAGE DeriveGeneric #-}
module BTypes where

import Types.Environment
import Types.Experiments

import GHC.Generics
import Data.Aeson

type Experiment = (Environment, String, Query, SearchParams, String)

data Query = Query {
  name :: String,
  query :: String
  } deriving (Generic, Show)

instance FromJSON Query

data ResultSummary = ResultSummary {
  envName :: String,
  paramName :: String,
  queryName :: String,
  queryStr :: String,
  solution :: String,
  tFirstSoln :: Double,
  tEncFirstSoln :: Double,
  lenFirstSoln :: Int,
  refinementSteps :: Int,
  transitions :: Int
  } deriving (Show, Eq)
