module Runner where

import BTypes
import BConfig
import Types.Program
import Types.Environment
import Types.Experiments
import Synquid.Util
import HooglePlus.Synthesize
import Database.Environment

import System.Timeout
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.ParallelIO.Local
import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import GHC.Conc (getNumCapabilities)
import GHC.Exception
import Data.Ratio ((%))

import System.IO.Unsafe

runExperiments :: ExperimentSetup -> [Experiment] -> IO [ResultSummary]
runExperiments setup exps = do
  maxThreads <- getNumCapabilities
  let threads = (ceiling ((fromIntegral (maxThreads * 3)) % (fromIntegral 4))) -- Use about 3/4 of the cores
  eitherMbResults <- withPool threads (runPool setup exps)
  let currentExperiment = expCourse setup
  return $ map (summarizeResult currentExperiment) (zip exps eitherMbResults)

runPool :: ExperimentSetup -> [Experiment] -> Pool -> IO [Either SomeException [(Maybe RProgram, TimeStatistics)]]
runPool setup exps pool = do
  parallelE pool (listOfExpsToDo exps)
  -- fmap (map Right) $ sequence $ listOfExpsToDo exps
  where
    listOfExpsToDo :: [Experiment] -> [IO [(Maybe RProgram, TimeStatistics)]]
    listOfExpsToDo = map (runExperiment setup)

runExperiment :: ExperimentSetup -> Experiment -> IO ([(Maybe RProgram, TimeStatistics)])
runExperiment setup (env, envName, q, params, paramName) = do
  let queryStr = query q
  let timeoutUs = (expTimeout setup * 10^6) -- Timeout in microseconds
  goal <- envToGoal env queryStr
  messageChan <- newChan
  forkIO $ do
    timeout timeoutUs $ synthesize params goal messageChan
    writeChan messageChan MesgClose -- could possibly be putting a second close on the channel.
  readChan messageChan >>= collectResults messageChan []


collectResults :: Chan Message -> [(Maybe RProgram, TimeStatistics)] -> Message -> IO [(Maybe RProgram, TimeStatistics)]
collectResults ch res MesgClose = return res
collectResults ch ((Nothing, _):xs) (MesgP (p, ts)) = readChan ch >>= (collectResults ch ((Just p, ts):xs))
collectResults ch xs (MesgP (p, ts)) = readChan ch >>= (collectResults ch ((Just p, ts):xs))
collectResults ch ((Nothing, _):xs) (MesgD ts) = readChan ch >>= (collectResults ch ((Nothing, ts):xs))
collectResults ch xs (MesgD ts) = readChan ch >>= (collectResults ch ((Nothing, ts):xs))


summarizeResult :: ExperimentCourse -> (Experiment, Either SomeException [(Maybe RProgram, TimeStatistics)]) -> ResultSummary
summarizeResult currentExperiment ((_, envN, q, _, paramN), r) = let
  results = case (currentExperiment, r) of
    (_, Left err) -> unsafePerformIO ((putStrLn (show err)) >> (return $ Left (RuntimeException err)))
    (_, Right []) -> Left TimeoutException
    (CompareInitialAbstractCovers, Right results) -> let
      safeTransitions = snd $ errorhead "missing transitions" $ (Map.toDescList (numOfTransitions firstR))
      safeTypes = snd $ errorhead "missing types" $ (Map.toDescList (numOfPlaces firstR))
      soln = mkOneLine (show solnProg)
      (solnProg, firstR) =  errorhead "missing first solution" results
      in Right Result {
      resSolution = soln,
      resTFirstSoln = totalTime firstR,
      resTEncFirstSoln = encodingTime firstR,
      resLenFirstSoln = pathLength firstR,
      resRefinementSteps = iterations firstR,
      resTransitions = [safeTransitions],
      resTypes = [safeTypes]
      }
    (TrackTypesAndTransitions, Right results) -> let
      safeTransitions = map snd (Map.toAscList (numOfTransitions firstR))
      safeTypes = map snd (Map.toAscList (numOfPlaces firstR))
      soln = mkOneLine (show solnProg)
      (solnProg, firstR) =  errorhead "missing first solution" results
      in Right Result {
      resSolution = soln,
      resTFirstSoln = totalTime firstR,
      resTEncFirstSoln = encodingTime firstR,
      resLenFirstSoln = pathLength firstR,
      resRefinementSteps = iterations firstR,
      resTransitions = safeTransitions,
      resTypes = safeTypes
      }

  in ResultSummary {
    envName = envN,
    paramName = paramN,
    queryName = name q,
    queryStr = query q,
    result = results
    }
  where
    errorhead msg xs = fromMaybe (error msg) $ listToMaybe xs
