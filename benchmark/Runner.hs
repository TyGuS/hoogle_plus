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
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.ParallelIO.Local
import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import GHC.Conc (getNumCapabilities)
import GHC.Exception
import Data.Ratio ((%))
import Data.List

import System.IO.Unsafe

runExperiments :: ExperimentSetup -> [Experiment] -> IO [ResultSummary]
runExperiments setup exps = do
  maxThreads <- getNumCapabilities
  let threads = (ceiling ((fromIntegral (maxThreads * 3)) % (fromIntegral 4))) -- Use about 3/4 of the cores
  eitherMbResults <- withPool threads (runPool setup exps)
  let currentExperiment = expCourse setup
  return $ map (summarizeResult currentExperiment) (zip exps eitherMbResults)

runPool :: ExperimentSetup -> [Experiment] -> Pool -> IO [[(Either EvaluationException (Maybe RProgram), TimeStatistics)]]
runPool setup exps pool = do
  nestedEithers <- parallelE pool (listOfExpsToDo exps)
  return $ map mergeEithers nestedEithers
  where
    listOfExpsToDo :: [Experiment] -> [IO [(Either EvaluationException (Maybe RProgram), TimeStatistics)]]
    listOfExpsToDo = map (runExperiment setup)
    mergeEithers :: Either SomeException [((Either EvaluationException b), TimeStatistics)] -> [(Either EvaluationException b, TimeStatistics)]
    mergeEithers (Left err) = [(Left (RuntimeException err), emptyTimeStats)]
    mergeEithers (Right rest) = rest

runExperiment :: ExperimentSetup -> Experiment -> IO [(Either EvaluationException (Maybe RProgram), TimeStatistics)]
runExperiment setup (env, envName, q, params, paramName) = do
  let queryStr = query q
  let timeoutUs = (expTimeout setup * 10^6) -- Timeout in microseconds
  goal <- envToGoal env queryStr
  messageChan <- newChan
  forkIO $ do
    timeout timeoutUs $ synthesize params goal messageChan
    writeChan messageChan (MesgClose CSTimeout) -- could possibly be putting a second close on the channel.
  readChan messageChan >>= collectResults messageChan []


-- collectResults will listen to a channel until it closes. Intermediate results are put on top
-- and replace existing intermediate results. Once a program/stats pair comes in, that will replace
-- any such intermediate results.
collectResults :: Chan Message -> [(Either EvaluationException (Maybe RProgram), TimeStatistics)] -> Message
                            -> IO [(Either EvaluationException (Maybe RProgram), TimeStatistics)]
collectResults ch res (MesgClose CSNormal) = return res
collectResults ch ((_,stats):xs) (MesgClose (CSError err)) = let
  -- This is a big of a hack
  errTy = if ("timeout" `isInfixOf` (show err)) then TimeoutException else RuntimeException err
  in return ((Left errTy, stats):xs)
collectResults ch ((_,stats):xs) (MesgClose CSTimeout) = return ((Left TimeoutException, stats):xs)
collectResults ch ((_,stats):xs) (MesgClose CSNoSolution) = return ((Left NoSolutionException, stats):xs)
collectResults ch res@((Left err, _):_) _ = return res
collectResults ch ((Right Nothing, _):xs) (MesgP (p, ts)) = readChan ch >>= (collectResults ch $ ((Right $ Just p, ts):xs))
collectResults ch xs (MesgP (p, ts)) = readChan ch >>= (collectResults ch $ ((Right $ Just p, ts):xs))
collectResults ch ((Right Nothing, _):xs) (MesgS ts) = readChan ch >>= (collectResults ch $ ((Right Nothing, ts):xs))
collectResults ch xs (MesgS ts) = readChan ch >>= (collectResults ch $ ((Right Nothing, ts):xs))
collectResults ch xs _ = readChan ch >>= (collectResults ch xs)


summarizeResult :: ExperimentCourse -> (Experiment, [(Either EvaluationException (Maybe RProgram), TimeStatistics)]) -> ResultSummary
summarizeResult currentExperiment ((_, envN, q, _, paramN), r) = let
  results = case (currentExperiment, r) of
    (_, []) -> emptyResult {resSolutionOrError = Left TimeoutException}
    (CompareInitialAbstractCovers, (errOrMbSoln, firstR):_) -> let
      safeTransitions = snd $ errorhead "missing transitions" $ (Map.toDescList (numOfTransitions firstR))
      safeTypes = snd $ errorhead "missing types" $ (Map.toDescList (numOfPlaces firstR))
      in Result {
      resSolutionOrError = fmap (mkOneLine . show) errOrMbSoln,
      resTFirstSoln = totalTime firstR,
      resTEncFirstSoln = encodingTime firstR,
      resLenFirstSoln = pathLength firstR,
      resRefinementSteps = iterations firstR,
      resTransitions = [safeTransitions],
      resTypes = [safeTypes]
      }
    (TrackTypesAndTransitions, (errOrMbSoln, firstR):_) -> let
      safeTransitions = map snd (Map.toAscList (numOfTransitions firstR))
      safeTypes = map snd (Map.toAscList (numOfPlaces firstR))
      in Result {
      resSolutionOrError = fmap (mkOneLine . show) errOrMbSoln,
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
