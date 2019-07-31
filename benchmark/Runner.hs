module Runner where

import BTypes
import BConfig
import Types.Program
import Types.Environment
import Types.Experiments
import Synquid.Util
import HooglePlus.Synthesize
import Database.Environment
import HooglePlus.Utils

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
import Data.List.Extra
import Data.Ratio ((%))
import Data.List
import Text.Printf
import System.IO.Unsafe
import System.Directory

runExperiments :: ExperimentSetup -> [Experiment] -> IO [ResultSummary]
runExperiments setup exps = do
  maxThreads <- getNumCapabilities
  let threads = ceiling (fromIntegral (maxThreads * 3) % 4) -- Use about 3/4 of the cores
  eitherMbResults <- withPool threads (runPool setup exps)
  let currentExperiment = expCourse setup
  return $ map (summarizeResult currentExperiment) (zip exps eitherMbResults)

runPool :: ExperimentSetup -> [Experiment] -> Pool -> IO [[(Either EvaluationException (Maybe RProgram), TimeStatistics)]]
runPool setup exps pool = do
  nestedEithers <- parallelE pool (listOfExpsToDo exps)
  return $ map mergeEithers nestedEithers
  where
    listOfExpsToDo :: [Experiment] -> [IO [(Either EvaluationException (Maybe RProgram), TimeStatistics)]]
    listOfExpsToDo xs = map (runExperiment setup) (zip xs $ zip [1..] $ repeat (length xs))
    mergeEithers :: Either SomeException [(Either EvaluationException b, TimeStatistics)] -> [(Either EvaluationException b, TimeStatistics)]
    mergeEithers (Left err) = [(Left (RuntimeException err), emptyTimeStats)]
    mergeEithers (Right rest) = rest

runExperiment :: ExperimentSetup -> (Experiment,(Int, Int)) -> IO [(Either EvaluationException (Maybe RProgram), TimeStatistics)]
runExperiment setup (exp@(env, envName, q, params, paramName), (n, total)) = do
  let queryStr = query q
  printf "Running [%d/%d]: (%s-%s): %s\n" n total envName paramName queryStr
  let timeoutUs = expTimeout setup * 10^6 -- Timeout in microseconds
  goal <- envToGoal env queryStr
  messageChan <- newChan
  forkIO $ do
    timeout timeoutUs $ synthesize params goal messageChan
    writeChan messageChan (MesgClose CSTimeout) -- could possibly be putting a second close on the channel.
  results <- (readChan messageChan >>= collectResults messageChan [])
  writeResult setup exp results
  return results

writeResult :: ExperimentSetup -> Experiment -> [(Either EvaluationException (Maybe RProgram), TimeStatistics)] -> IO ()
writeResult setup (env, envName, q, params, paramName) xs = do
    let solns = reverse xs -- They come in reverse order
    let fileName = (printf "%s+%s.tsv" (replace " " "-" $ paramName) (replace " " "-" $ name q)) :: String
    let dirName = (expDirectory setup) ++ "/"
    createDirectoryIfMissing True dirName
    let rowHeaders =
            [ "name"
            , "query"
            , "encodingTime"
            , "constructionTime"
            , "solverTime"
            , "codeFormerTime"
            , "refineTime"
            , "typeCheckerTime"
            , "totalTime"
            , "iterations"
            , "pathLength"
            , "numOfTransitions"
            , "numOfPlaces"
            , "duplicateSymbols"
            , "Solution"
            ]
    let rowsdata = map showRow solns
    let fileData = rowHeaders : rowsdata
    let rows = (map (intercalate "\t") fileData) :: [String]
    let fileStr = ((intercalate "\r\n") rows) :: String
    writeFile (dirName ++ fileName) (fileStr)

    where
      showResult (Left err) = show err
      showResult (Right Nothing) = "No Solution"
      showResult (Right (Just x)) = toHaskellSolution $ show x

      showRow (errOrMbProg, times) =
         [ (name q)
         , (query q)
         , showFloat $ encodingTime times
         , showFloat $ constructionTime times
         , showFloat $ solverTime times
         , showFloat $ codeFormerTime times
         , showFloat $ refineTime times
         , showFloat $ typeCheckerTime times
         , showFloat $ totalTime times
         , show $ iterations times
         , show $ pathLength times
         , show $ map snd $ Map.toAscList $ numOfTransitions times
         , show $ map snd $ Map.toAscList $ numOfPlaces times
         , show $ duplicateSymbols times
         , showResult errOrMbProg
         ]



-- collectResults will listen to a channel until it closes. Intermediate results are put on top
-- and replace existing intermediate results. Once a program/stats pair comes in, that will replace
-- any such intermediate results.
collectResults :: Chan Message -> [(Either EvaluationException (Maybe RProgram), TimeStatistics)] -> Message
               -> IO [(Either EvaluationException (Maybe RProgram), TimeStatistics)]
collectResults ch res (MesgClose CSNormal) = return res
collectResults ch ((_,stats):xs) (MesgClose CSTimeout) = return ((Left TimeoutException, stats):xs)
collectResults ch ((_,stats):xs) (MesgClose CSNoSolution) = return ((Left NoSolutionException, stats):xs)
collectResults ch xs (MesgClose (CSError err)) = let
  -- This is a big of a hack
  errTy = if "timeout" `isInfixOf` show err then TimeoutException else RuntimeException err
  stats = case xs of
    (_, existingStats):_ -> existingStats
    _ -> emptyTimeStats
  in return ((Left errTy, stats):xs)
collectResults ch res@((Left err, _):_) _ = return res
collectResults ch ((Right Nothing, _):xs) (MesgP (p, ts)) = printSolution p >> readChan ch >>= collectResults ch ((Right $ Just p, ts):xs)
collectResults ch xs (MesgP (p, ts)) = printSolution p >> readChan ch >>= collectResults ch ((Right $ Just p, ts):xs)
collectResults ch ((Right Nothing, _):xs) (MesgS ts) = readChan ch >>= collectResults ch ((Right Nothing, ts):xs)
collectResults ch xs (MesgS ts) = readChan ch >>= collectResults ch ((Right Nothing, ts):xs)
collectResults ch xs _ = readChan ch >>= collectResults ch xs

summarizeResult :: ExperimentCourse
                -> (Experiment, [(Either EvaluationException (Maybe RProgram), TimeStatistics)])
                -> ResultSummary
summarizeResult currentExperiment ((_, envN, q, _, paramN), r) = let
  results = case (currentExperiment, r) of
    (_, []) -> [emptyResult {resSolutionOrError = Left TimeoutException}]
    -- We want all the solutions
    (_, solns) -> map outputToResult solns

  in ResultSummary {
    envName = envN,
    paramName = paramN,
    queryName = name q,
    queryStr = query q,
    results = results
    }
  where
    errorhead msg xs = fromMaybe (error msg) $ listToMaybe xs

    outputToResult :: (Either EvaluationException (Maybe RProgram), TimeStatistics) -> Result
    outputToResult (soln,stats) = let
      safeTransitions = map snd (Map.toAscList (numOfTransitions stats))
      safeTypes = map snd (Map.toAscList (numOfPlaces stats))
      solution = either Left (maybe (Left NoSolutionException) (Right . toHaskellSolution . show)) soln
      in emptyResult {
        resSolutionOrError = solution,
        resTFirstSoln = totalTime stats,
        resTEncFirstSoln = encodingTime stats,
        resLenFirstSoln = pathLength stats,
        resRefinementSteps = iterations stats,
        resTransitions = safeTransitions,
        resTypes = safeTypes,
        resDuplicateSymbols = duplicateSymbols stats
        }

