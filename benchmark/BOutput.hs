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
    columnStyle = [
      column expand center dotAlign (singleCutMark "..."),
      column expand center dotAlign (singleCutMark "...")] ++ dataColumnStyle
    header = ["Name", "Query"] ++ dataHeader
    dataColumnStyle = replicate (length dataHeader) (column (expandUntil 50) center dotAlign (singleCutMark "..."))
    dataHeader =  [
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
      either show (showFloat . resTFirstSoln) <$> result <$> mbqr,
      either dash (showFloat . resTEncFirstSoln) <$> result <$> mbqr,
      either dash (show . resLenFirstSoln) <$> result <$> mbqr,
      either dash (show . resRefinementSteps) <$> result <$> mbqr,
      either dash (show . resTransitions) <$> result <$> mbqr,
      either dash (show . resTypes) <$> result <$> mbqr,
      either show (showFloat . resTFirstSoln) <$> result <$> mbqrhof,
      either dash (show . resTransitions) <$> result <$> mbqrhof,
      either show (showFloat . resTFirstSoln) <$> result <$> mbbaseline,
      either dash (show . resTransitions) <$> result <$> mbbaseline,
      either show (showFloat . resTFirstSoln) <$> result <$> mbzero,
      either dash (show . resTransitions) <$> result <$> mbzero
     ]
  where
    findwhere name xs = find ((==) name . paramName) xs
    realizeMb mbX = fromMaybe "-" mbX
    dash = const "-"
