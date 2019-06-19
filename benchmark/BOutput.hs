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
-- import Data.List.Utils (replace)
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
toTable CompareSolutions rsMap = let
  header = ["Name", "Query", "with demand", "no demand"]
  columnStyle = replicate (length header) (column expand center noAlign (singleCutMark "..."))
  -- body = map (uncurry . (collsAllG center) . (toLine CompareSolutions)) (Map.toList rsMap)
  body = map (\(x, y) -> colsAllG center $ toLine CompareSolutions x y) (Map.toList rsMap)
  in
    tableString columnStyle unicodeRoundS (titlesH header) body
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
        "tS - B", "tr - B",
        "tS - Z", "tr - Z"
        ]
      TrackTypesAndTransitions -> [
        "time", "encoding - s",
        "l", "r", "transitions", "types", "duplicate symbols", "status"]

toLine :: ExperimentCourse -> String -> [ResultSummary] -> [Col String]
toLine currentExperiment name rss = let
  mbqr = findwhere expTyGarQ rss
  mbbaseline = findwhere expSypetClone rss
  mbzero = findwhere expTyGar0 rss
  rest = case currentExperiment of
      CompareInitialAbstractCovers -> [
        (:[]) <$> (showFloat . resTFirstSoln) <$> (head . results) <$> mbqr,
        (:[]) <$> (showFloat . resTEncFirstSoln) <$> (head . results) <$> mbqr,
        (:[]) <$> (show . resLenFirstSoln) <$> (head . results) <$> mbqr,
        (:[]) <$> (show . resRefinementSteps) <$> (head . results) <$> mbqr,
        (:[]) <$> (show . resTransitions) <$> (head . results) <$> mbqr,
        (:[]) <$> (show . resTypes) <$> (head . results) <$> mbqr,

        (:[]) <$> (showFloat . resTFirstSoln) <$> (head . results) <$> mbbaseline,
        (:[]) <$> (show . resTransitions) <$> (head . results) <$> mbbaseline,
        (:[]) <$> (showFloat . resTFirstSoln) <$> (head . results) <$> mbzero,
        (:[]) <$> (show . resTransitions) <$> (head . results) <$> mbzero
        ]
      TrackTypesAndTransitions -> [
        justifyText textWidth <$> (showFloat . resTFirstSoln) <$> (head . results) <$> mbqr,
        justifyText textWidth <$> (showFloat . resTEncFirstSoln) <$> (head . results) <$> mbqr,
        justifyText textWidth <$> (show . resLenFirstSoln) <$> (head . results) <$> mbqr,
        justifyText textWidth <$> (show . resRefinementSteps) <$> (head . results) <$> mbqr,
        justifyText textWidth <$> (spaceList . resTransitions . (head . results)) <$> mbqr,
        justifyText textWidth <$> (spaceList . resTypes . (head . results)) <$> mbqr,
        justifyText textWidth <$> (spaceList . resDuplicateSymbols . (head . results)) <$> mbqr,
        justifyText textWidth <$> either show show <$> resSolutionOrError <$> (head . results) <$> mbqr
        ]
      CompareSolutions -> let
          mbQueryRefinementNoDmd = findwhere expTyGarQNoDmd rss
          queryRefinementResults = (fromJust (results <$> mbqr)) :: [Result]
          queryRefinementResultsNoDmd = (fromJust (results <$> mbQueryRefinementNoDmd)) :: [Result]
          toSolution = (either show show . resSolutionOrError) :: Result -> String
          myResults = [queryRefinementResults, queryRefinementResultsNoDmd]
        in
          map (\x -> Just (map (mkOneLine . toSolution) (reverse x))) myResults

  in
    map (fromMaybe ["-"]) ([
      Just [name],
      justifyText textWidth <$> queryStr <$> mbqr
      ] ++ rest)
  where
    findwhere name = find ((==) name . paramName)
    dash = const "-"
    spaceList :: Show a => [a] -> String
    spaceList xs = intercalate ", " $ splitBy ',' $ show xs


toTabling :: ExperimentCourse -> Map String [ResultSummary] -> [(String, [String])]
toTabling CompareSolutions rsMap = let
  header = ["Name", "Query", "With demand", "No demand"]
  body = map (toRow CompareSolutions) (Map.toList rsMap)
  bodyT = transpose body
  in
    zip header bodyT

toRow :: ExperimentCourse -> (String, [ResultSummary]) -> [String]
toRow currentExp (name, rss) = let
  mbqr = findwhere expTyGarQ rss
  mbPartial = find (\x -> (paramName x == expTyGarQ) && (envName x == "Partial"))  rss
  mbPartialNoDmd = find (\x -> (paramName x == expTyGarQNoDmd) && (envName x == "Partial"))  rss
  rest = case currentExp of
    CompareSolutions -> let
        queryRefinementResults = (fromJust (results <$> mbPartial)) :: [Result]
        queryRefinementResultsNoDmd = (fromJust (results <$> mbPartialNoDmd)) :: [Result]
        toSolution = (either show id . resSolutionOrError) :: Result -> String
        myResults = [queryRefinementResults, queryRefinementResultsNoDmd]
      in
        map (\x -> Just $ unlines (map (mkOneLine . toSolution) (reverse x))) myResults
  in
    map (fromMaybe "-") ([
      Just name,
      queryStr <$> mbPartial
      ] ++ rest)
  where
    findwhere name = find ((==) name . paramName)

toAsciiTable :: [(String, [String])] -> String
toAsciiTable table = let
  columnStyles = foldr ((:) . addColumnStyle . fst) [] table
  addColumnStyle _ = column expand center noAlign (singleCutMark "...")
  headers = map fst table
  body = map (\(_, b) -> colsAllG center $ map (justifyText textWidth) b) table
  in tableString columnStyles unicodeRoundS (titlesH headers) body

toTSV :: [(String, [String])] -> String
toTSV table = let
  headers = map fst table
  body = map (\y -> map (\x -> "\"" ++ (replace "\"" "\\\"" x ++ "\"")) $ snd y) table
  bodyT = transpose body
  tsvBody = [headers] ++ bodyT
  rows = (map (intercalate "\t") tsvBody)
  in
    intercalate "\r\n" rows

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
  outputSummary Table CompareSolutions = toAsciiTable . (toTabling CompareSolutions) . toGroup
  outputSummary TSV CompareSolutions = toTSV . (toTabling CompareSolutions) . toGroup
  outputSummary Table exp = (toTable exp) . toGroup
