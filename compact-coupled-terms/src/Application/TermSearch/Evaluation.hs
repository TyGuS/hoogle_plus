module Application.TermSearch.Evaluation
    ( runEval
    , runBenchmark
    ) where

import           Control.Monad                  ( forM_ )
import           Data.Time                      ( diffUTCTime
                                                , getCurrentTime
                                                )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           System.Timeout

import qualified Data.Bifunctor                as Bi
import qualified Data.Text                     as Text

import           Data.ECTA
import           Data.ECTA.Term

import           Application.TermSearch.Dataset
import           Application.TermSearch.TermSearch
import           Application.TermSearch.Type
import           Application.TermSearch.Utils

runBenchmark :: Benchmark -> AblationType -> Int -> IO ()
runBenchmark (Benchmark name size sol args res) ablation limit = do
    putStrLn $ "Running benchmark " ++ Text.unpack name

    let argNodes = map (Bi.bimap Symbol typeToFta) args
    let resNode  = typeToFta res

    start <- getCurrentTime
    _ <- timeout (limit * 10 ^ (6 :: Int)) $ forM_ [1..size] $ synthesize argNodes resNode
    end <- getCurrentTime
    print $ "Time: " ++ show (diffUTCTime end start)
    hFlush stdout

  where
    synthesize :: [Argument] -> Node -> Int -> IO ()
    synthesize argNodes resNode sz = do
      let anyArg   = Node (map (uncurry constArg) argNodes)
      let !filterNode = filterType (relevantTermsOfSize anyArg argNodes sz) resNode
      case ablation of
          NoReduction -> do
              prettyPrintAllTerms ablation (substTerm sol) filterNode
          NoOptimize  -> do
              prettyPrintAllTerms ablation (substTerm sol) filterNode
          _           -> do
              reducedNode <- reduceFullyAndLog filterNode
              -- let reducedNode = reduceFully filterNode
              let foldedNode = refold reducedNode
              prettyPrintAllTerms ablation (substTerm sol) foldedNode

runEval :: IO ()
runEval = undefined -- mapM_ runBenchmark hoogleplusBenchmarks
