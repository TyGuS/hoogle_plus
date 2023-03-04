{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List ( nub )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO ( hFlush, stdout )

import System.Console.CmdArgs ( Data, Typeable, cmdArgs, argPos, auto, (&=), help )

import Data.ECTA
import Data.ECTA.Internal.ECTA.Enumeration
import Data.ECTA.Term
import Data.Persistent.UnionFind
import Application.TermSearch.Evaluation
import Application.TermSearch.Type

----------------------------------------------------------

printAllEdgeSymbols :: Node -> IO ()
printAllEdgeSymbols n = print $ nub $ crush (onNormalNodes $ \(Node es) -> map edgeSymbol es) n

printCacheStatsForReduction :: Node -> IO ()
printCacheStatsForReduction n = do
    let n' = reducePartially n
    Text.putStrLn $ "Nodes: "        <> Text.pack (show (nodeCount   n'))
    Text.putStrLn $ "Edges: "        <> Text.pack (show (edgeCount   n'))
    Text.putStrLn $ "Max indegree: " <> Text.pack (show (maxIndegree n'))
#ifdef PROFILE_CACHES
    Memoization.printAllCacheMetrics
    Text.putStrLn =<< (pretty <$> Interned.getMetrics (cache @Node))
    Text.putStrLn =<< (pretty <$> Interned.getMetrics (cache @Edge))
    Text.putStrLn ""
#endif
    hFlush stdout


getTermsNoOccursCheck :: Node -> [Term]
getTermsNoOccursCheck n = map (termFragToTruncatedTerm . fst) $
                          flip runEnumerateM (initEnumerationState Nothing n) $ do
                            _ <- enumerateOutUVar (intToUVar 0)
                            getTermFragForUVar    (intToUVar 0)

--------------------------------------------------------------------------------

data HPPArgs = HPPArgs { benchmark    :: String
                       , ablation     :: AblationType
                       , timeoutLimit :: Int
                       }
  deriving (Data, Typeable)

hppArgs :: HPPArgs
hppArgs = HPPArgs {
    benchmark = "" &= argPos 0
  , ablation  = Default &= help "Ablation type. choices: [default, no-reduction, no-enumeration]"
  , timeoutLimit = 300 &= help "Timeout limit in seconds"
  } &= auto


main :: IO ()
main = do
    query <- cmdArgs hppArgs
    runBenchmark (read $ benchmark query) (ablation query) (timeoutLimit query)