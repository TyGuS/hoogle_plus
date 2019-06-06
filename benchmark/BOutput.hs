{-# LANGUAGE FlexibleInstances #-}
module BOutput where

import BTypes
import BConfig
import Synquid.Util
import Types.Experiments
import Types.Environment

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.List
import Data.List.Extra
import Data.Maybe
import Text.Layout.Table
import Control.Exception

textWidth = 50

toGroup :: [ResultSummary] -> Map String [ResultSummary]
toGroup rss = let
  updateMap rs qsMap = Map.alter (\mbQs -> Just ((updateRs rs) mbQs)) (queryName rs) qsMap
  updateRs rs Nothing = [rs]
  updateRs rs (Just others) = rs:others
  in
    foldr updateMap Map.empty rss

toTable :: ExperimentCourse -> Map String [ResultSummary] -> String
toTable currentExperiment rsMap = let
  body = map (\(x, y) -> colsAllG center $ toLine currentExperiment x y) (Map.toList rsMap)
  in
    tableString columnStyle unicodeRoundS (titlesH header) body
  where
    columnStyle = [
      column expand center dotAlign (singleCutMark "..."),
      column expand center dotAlign (singleCutMark "...")] ++ dataColumnStyle
    header = ["Name", "Query"] ++ dataHeader
    dataColumnStyle = replicate (length dataHeader) (column (expandUntil textWidth) center noAlign (singleCutMark "..."))
    dataHeader = case currentExperiment of
      CompareInitialAbstractCovers -> [
        "tS - QR", "tEnc - QR",
        "l", "r", "tr", "ty",
        "tS - QRHOF", "tr - QRHOF",
        "partial - tS - QRHOF", "partial - tr - QRHOF",
        "tS - B", "tr - B",
        "tS - Z", "tr - Z"
        ]
      TrackTypesAndTransitions -> [
        "time", "encoding - s",
        "l", "r", "transitions", "types", "duplicate symbols", "status"]

toLine :: ExperimentCourse -> String -> [ResultSummary] -> [Col String]
toLine currentExperiment name rss = let
  mbqr = findwhere expQueryRefinement rss
  mbbaseline = findwhere expBaseline rss
  mbqrhof = find (\x -> (paramName x == expQueryRefinementHOF) && (envName x == "Total")) rss
  mbPartialqrhof = find (\x -> (paramName x == expQueryRefinementHOF) && (envName x == "Partial"))  rss
  mbzero = findwhere expZeroCoverStart rss
  rest = case currentExperiment of
      CompareInitialAbstractCovers -> [
        (:[]) <$> (showFloat . resTFirstSoln) <$> result <$> mbqr,
        (:[]) <$> (showFloat . resTEncFirstSoln) <$> result <$> mbqr,
        (:[]) <$> (show . resLenFirstSoln) <$> result <$> mbqr,
        (:[]) <$> (show . resRefinementSteps) <$> result <$> mbqr,
        (:[]) <$> (show . resTransitions) <$> result <$> mbqr,
        (:[]) <$> (show . resTypes) <$> result <$> mbqr,

        (:[]) <$> (showFloat . resTFirstSoln) <$> result <$> mbqrhof,
        (:[]) <$> (show . resTransitions) <$> result <$> mbqrhof,

        (:[]) <$> (showFloat . resTFirstSoln) <$> result <$> mbPartialqrhof,
        (:[]) <$> (show . resTransitions) <$> result <$> mbPartialqrhof,

        (:[]) <$> (showFloat . resTFirstSoln) <$> result <$> mbbaseline,
        (:[]) <$> (show . resTransitions) <$> result <$> mbbaseline,
        (:[]) <$> (showFloat . resTFirstSoln) <$> result <$> mbzero,
        (:[]) <$> (show . resTransitions) <$> result <$> mbzero
        ]
      TrackTypesAndTransitions -> [
        justifyText textWidth <$> (showFloat . resTFirstSoln) <$> result <$> mbqr,
        justifyText textWidth <$> (showFloat . resTEncFirstSoln) <$> result <$> mbqr,
        justifyText textWidth <$> (show . resLenFirstSoln) <$> result <$> mbqr,
        justifyText textWidth <$> (show . resRefinementSteps) <$> result <$> mbqr,
        justifyText textWidth <$> (spaceList . show . resTransitions . result) <$> mbqr,
        justifyText textWidth <$> (spaceList . show . resTypes . result) <$> mbqr,
        justifyText textWidth <$> (show . resDuplicateSymbols . result) <$> mbqr,
        justifyText textWidth <$> either show show <$> resSolutionOrError <$> result <$> mbqr
        ]
  in
    map (fromMaybe ["-"]) ([
      Just [name],
      justifyText textWidth <$> show . queryStr <$> mbqr
      ] ++ rest)
  where
    findwhere name xs = find ((==) name . paramName) xs
    dash = const "-"
    spaceList xs = intercalate ", " $ splitBy ',' xs


toEnvTable :: [(Environment, String)] -> String
toEnvTable envAndNames = let
  body = map toEnvLine envAndNames
  in
    tableString columnStyle unicodeRoundS (titlesH header) body
  where
    columnStyle = [
      column (expandUntil 50) center dotAlign (singleCutMark "..."),
      column (expandUntil 50) center dotAlign (singleCutMark "..."),
      column (expandUntil 50) center dotAlign (singleCutMark "..."),
      column expand center dotAlign (singleCutMark "...")]
    header = ["Name", "Functions", "Types", "Modules"]
    toEnvLine (env, name) =
      colsAllG center [
        [name],
        [show $ length $ concatMap Map.elems $ Map.elems $ _symbols env],
        [show $ length $ Map.keys $ _datatypes env],
        justifyText textWidth $ (replace "," " " $ show $ Set.toList $ _included_modules env)
      ]

instance Summary [(Environment, String)] where
  outputSummary Table _ = toEnvTable

instance Summary [ResultSummary] where
  outputSummary Table exp = (toTable exp) . toGroup
