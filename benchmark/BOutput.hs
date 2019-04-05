module BOutput where

import BTypes
import BConfig
import Synquid.Util
import Types.Experiments

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe
import Text.Layout.Table

toGroup :: [ResultSummary] -> Map String [ResultSummary]
toGroup rss = let
  updateMap rs qsMap = Map.alter (\mbQs -> Just ((updateRs rs) mbQs)) (queryName rs) qsMap
  updateRs rs Nothing = [rs]
  updateRs rs (Just others) = rs:others
  in
    foldr updateMap Map.empty rss

toTable :: Map String [ResultSummary] -> String
toTable rsMap = let
  body = map (\(x, y) -> rowG $ toLine x y) (Map.toList rsMap)
  in
    tableString columnStyle unicodeRoundS (titlesH header) body
  where
    columnStyle = replicate (length header) (column expand center dotAlign (singleCutMark "..."))
    header =  [
      "Name", "Query",
      "tS - QR", "tEnc - QR",
      "l", "r", "tr", "ty",
      "tS - QRHOF", "tr - QRHOF",
      "tS - B", "tr - B",
      "tS - Z", "tr - Z"
      ]

toLine :: String -> [ResultSummary] -> [String]
toLine name rss = let
  mbqr = findwhere expQueryRefinement rss
  mbqrhof = findwhere expQueryRefinementHOF rss
  mbbaseline = findwhere expBaseline rss
  mbzero = findwhere expZeroCoverStart rss
  in
    map realizeMb [
      Just name,
      show . queryStr <$> mbqr,
      showFloat . tFirstSoln <$> mbqr,
      showFloat . tEncFirstSoln <$> mbqr,
      show . lenFirstSoln <$> mbqr,
      show . refinementSteps <$> mbqr,
      show . transitions <$> mbqr,
      show . rsTypes <$> mbqr,
      showFloat . tFirstSoln <$> mbqrhof,
      show . transitions <$> mbqrhof,
      showFloat . tFirstSoln <$> mbbaseline,
      show . transitions <$> mbbaseline,
      showFloat . tFirstSoln <$> mbzero,
      show . transitions <$> mbzero
     ]
  where
    findwhere name xs = find ((==) name . paramName) xs
    realizeMb mbX = fromMaybe "-" mbX
