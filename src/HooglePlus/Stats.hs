{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.Stats where

import Types.Experiments
import Types.Solver
import Types.CheckMonad

import Synquid.Util
import Control.Monad.State
import System.CPUTime
import Text.Printf
import Control.Lens
import qualified Data.Map as Map
import Text.Pretty.Simple

-- | wrap some action with time measuring and print out the execution time
withTime :: (CheckMonad (t m), MonadIO m) => TimeStatUpdate -> t m a -> t m a
withTime desc f = do
    start <- liftIO getCPUTime
    res <- f
    end <- liftIO getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    overStats (\s ->
        case desc of
          ConstructionTime -> s { _constructionTime = _constructionTime s + (diff :: Double) }
          EncodingTime -> s { _encodingTime = _encodingTime s + (diff :: Double) }
          FormerTime -> s { _codeFormerTime = _codeFormerTime s + (diff :: Double) }
          SolverTime -> s { _solverTime = _solverTime s + (diff :: Double) }
          RefinementTime -> s { _refineTime = _refineTime s + (diff :: Double) }
          TypeCheckTime -> s { _typeCheckerTime = _typeCheckerTime s + (diff :: Double) }
          TotalSearch -> s { _totalTime = _totalTime s + (diff :: Double) }
        )
    return res

resetTiming :: Monad m => PNSolver m ()
resetTiming =
  modify $ over (statistics . solverStats) (\s ->
    s { _encodingTime=0,
        _codeFormerTime=0,
        _solverTime=0,
        _refineTime=0,
        _typeCheckerTime=0,
        _totalTime=0
      })

printStats :: MonadIO m => PNSolver m ()
printStats = do
    stats <- gets $ view (statistics . solverStats)
    depth <- gets $ view (searchState . currentLoc)
    liftIO $ putStrLn "*******************STATISTICS*******************"
    liftIO $ putStrLn ("Search time for solution: " ++ showFullPrecision (_totalTime stats))
    liftIO $ putStrLn ("Petri net construction time: " ++ showFullPrecision (_constructionTime stats))
    liftIO $ putStrLn ("Petri net encoding time: " ++ showFullPrecision (_encodingTime stats))
    liftIO $ putStrLn ("Z3 solving time: " ++ showFullPrecision (_solverTime stats))
    liftIO $ putStrLn ("Hoogle plus code former time: " ++ showFullPrecision (_codeFormerTime stats))
    liftIO $ putStrLn ("Hoogle plus refinement time: " ++ showFullPrecision (_refineTime stats))
    liftIO $ putStrLn ("Hoogle plus type checking time: " ++ showFullPrecision (_typeCheckerTime stats))
    liftIO $ putStrLn ("Total iterations of refinements: " ++ show (_iterations stats))
    liftIO $ putStrLn ("Number of places: " ++ show (map snd (Map.toAscList (_numOfPlaces stats))))
    liftIO $ putStrLn ("Number of transitions: " ++ show (map snd (Map.toAscList (_numOfTransitions stats))))
    liftIO $ putStrLn ("Solution Depth: " ++ show depth)
    liftIO $ putStrLn "********************END STATISTICS****************************"


printTime :: TimeStatistics -> IO ()
printTime = pPrint
