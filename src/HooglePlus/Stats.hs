{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.Stats where

import Types.Experiments
import Types.Solver
import Types.Common
import Synquid.Util
import Synquid.Pretty
import PetriNet.Util

import Control.Monad.State
import System.CPUTime
import Text.Printf
import Control.Lens
import qualified Data.Map as Map
import Text.Pretty.Simple

-- | wrap some action with time measuring and print out the execution time
withTime :: MonadIO m => TimeStatUpdate -> PNSolver m a -> PNSolver m a
withTime desc f = do
    start <- liftIO getCPUTime
    res <- f
    end <- liftIO getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    stats <- gets getTimeStats
    modify $ setTimeStats $ case desc of
          ConstructionTime -> stats { constructionTime = constructionTime stats + (diff :: Double) }
          EncodingTime -> stats { encodingTime = encodingTime stats + (diff :: Double) }
          FormerTime -> stats { codeFormerTime = codeFormerTime stats + (diff :: Double) }
          SolverTime -> stats { solverTime = solverTime stats + (diff :: Double) }
          RefinementTime -> stats { refineTime = refineTime stats + (diff :: Double) }
          TypeCheckTime -> stats { typeCheckerTime = typeCheckerTime stats + (diff :: Double) }
          TotalSearch -> stats {totalTime = totalTime stats + (diff :: Double) }
    return res

resetTiming :: Monad m => PNSolver m ()
resetTiming =
  overSolver solverStats (\s ->
    s { encodingTime=0,
        codeFormerTime=0,
        solverTime=0,
        refineTime=0,
        typeCheckerTime=0,
        totalTime=0
      })

printStats :: MonadIO m => PNSolver m ()
printStats = do
    stats <- getSolver solverStats
    depth <- getSolver currentLoc
    liftIO $ putStrLn "*******************STATISTICS*******************"
    liftIO $ putStrLn ("Search time for solution: " ++ showFullPrecision (totalTime stats))
    liftIO $ putStrLn ("Petri net construction time: " ++ showFullPrecision (constructionTime stats))
    liftIO $ putStrLn ("Petri net encoding time: " ++ showFullPrecision (encodingTime stats))
    liftIO $ putStrLn ("Z3 solving time: " ++ showFullPrecision (solverTime stats))
    liftIO $ putStrLn ("Hoogle plus code former time: " ++ showFullPrecision (codeFormerTime stats))
    liftIO $ putStrLn ("Hoogle plus refinement time: " ++ showFullPrecision (refineTime stats))
    liftIO $ putStrLn ("Hoogle plus type checking time: " ++ showFullPrecision (typeCheckerTime stats))
    liftIO $ putStrLn ("Total iterations of refinements: " ++ show (iterations stats))
    liftIO $ putStrLn ("Number of places: " ++ show (map snd (Map.toAscList (numOfPlaces stats))))
    liftIO $ putStrLn ("Number of transitions: " ++ show (map snd (Map.toAscList (numOfTransitions stats))))
    liftIO $ putStrLn ("Solution Depth: " ++ show depth)
    liftIO $ putStrLn "********************END STATISTICS****************************"


printTime :: TimeStatistics -> IO ()
printTime = pPrint
