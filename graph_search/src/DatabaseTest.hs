{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.State
import qualified Data.Map as Map

import Database.Generate
import Database.Convert
import Database.Download
import Database.Graph
import Synquid.Type
import Synquid.Pretty
import Synquid.Program
import Synquid.SolverMonad

convertTest = printDeclarations "bytestring" (Just "0.10.8.1")

downloadTest = do
    downloadFile "bytestring" (Just "0.10.8.1")
    downloadFile "bytestring" Nothing

graphTest :: MonadIO m => StateT EnvState m Environment
graphTest = do
    packageEnv "bytestring"
    -- writeLog 3 $ text $ showGraphViz env

main = do
    -- downloadTest
    -- convertTest
    env <- evalStateT graphTest Map.empty
    putStrLn $ showGraphViz env