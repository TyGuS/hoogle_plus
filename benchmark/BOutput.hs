module BOutput where

import BTypes
import BConfig

import qualified Data.Map as Map
import Data.Map (Map)

toGroup :: [ResultSummary] -> Map String [ResultSummary]
toGroup rss = let
  updateMap rs qsMap = Map.alter (\mbQs -> Just ((updateRs rs) mbQs)) (queryName rs) qsMap
  updateRs rs Nothing = [rs]
  updateRs rs (Just others) = rs:others
  in
    foldr updateMap Map.empty rss
