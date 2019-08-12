{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.Stats where

import Types.Experiments
import Types.Solver
import Synquid.Util
import Synquid.Pretty

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
    modify $ over solverStats (\s ->
        case desc of
          ConstructionTime -> s { constructionTime = constructionTime s + (diff :: Double) }
          EncodingTime -> s { encodingTime = encodingTime s + (diff :: Double) }
          FormerTime -> s { codeFormerTime = codeFormerTime s + (diff :: Double) }
          SolverTime -> s { solverTime = solverTime s + (diff :: Double) }
          RefinementTime -> s { refineTime = refineTime s + (diff :: Double) }
          TypeCheckTime -> s { typeCheckerTime = typeCheckerTime s + (diff :: Double) }
          TotalSearch -> s {totalTime = totalTime s + (diff :: Double) }
        )
    return res

resetTiming :: Monad m => PNSolver m ()
resetTiming =
  modify $ over solverStats (\s ->
    s { encodingTime=0,
        codeFormerTime=0,
        solverTime=0,
        refineTime=0,
        typeCheckerTime=0,
        totalTime=0
      })

printStats :: MonadIO m => PNSolver m ()
printStats = do
    stats <- gets $ view solverStats
    depth <- gets $ view currentLoc
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
