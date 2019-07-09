{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module BTypes where

import Types.Environment
import Types.Experiments
import Types.Generate

import GHC.Generics
import GHC.Exception
import Data.Aeson hiding (Result)
import Data.Data
import Data.Typeable
import Data.Either
import Control.Exception

data Args = Args {
  argsQueryFile :: String,
  argsTimeout :: Int, -- Timeout in seconds
  argsOutputFile :: [FilePath],
  argsExperiment :: ExperimentCourse,
  argsOutputFormat :: [ResultFormat],
  argsPreset :: Preset
  } deriving (Show, Data, Typeable)

data ExperimentSetup = ExpSetup {
  expTimeout :: Int, -- Timeout in seconds
  expCourse :: ExperimentCourse
  }

type Experiment = (Environment, String, Query, SearchParams, String)

data Query = Query {
  name :: String,
  query :: String
  } deriving (Generic, Show)

instance FromJSON Query

class Summary a where
  outputSummary :: ResultFormat -> ExperimentCourse -> a -> String

data ResultSummary = ResultSummary {
  envName :: String,
  paramName :: String,
  queryName :: String,
  queryStr :: String,
  results :: [Result]
  } deriving (Show)

data Result = Result {
  resSolutionOrError :: Either EvaluationException String,
  resTFirstSoln :: Double,
  resTEncFirstSoln :: Double,
  resLenFirstSoln :: Int,
  resRefinementSteps :: Int,
  resTransitions :: [Int],
  resTypes :: [Int],
  resDuplicateSymbols :: [(Int, Int, Int)]
  } deriving (Show)

emptyResult = Result (Left NotImplementedException) 0 0 0 0 [] [] []

data EvaluationException =
  TimeoutException
  | NoSolutionException
  | RuntimeException SomeException
  | NotImplementedException

data ResultFormat = Table | TSV | Latex | Plot | None deriving (Show, Data, Typeable)

instance Show EvaluationException where
  show TimeoutException = "Timeout"
  show NoSolutionException = "No Solution"
  show NotImplementedException = "Not Implemented"
  show (RuntimeException _) = "Runtime error"
instance Exception EvaluationException
