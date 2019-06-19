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

toTabling :: ExperimentCourse -> Map String [ResultSummary] -> [(String, [String])]
toTabling exp rsMap = let
  body = map (toRow exp) (Map.toList rsMap)
  bodyT = transpose body
  headers = ["Name", "Query"] ++ headersList exp
  in
    zip headers bodyT

toAsciiTable :: [(String, [String])] -> String
toAsciiTable table = let
  columnStyles = foldr ((:) . addColumnStyle . fst) [] table
  addColumnStyle _ = column expand center noAlign (singleCutMark "...")
  headers = map fst table
  tableData = transpose $ map snd table
  body = map (\b -> colsAllG center $ map (justifyText textWidth) b) tableData
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
  outputSummary Table CompareSolutions = error "Use TSV; theres a library bug here."
  outputSummary Table exp = toAsciiTable . (toTabling exp) . toGroup
  outputSummary TSV exp = toTSV . (toTabling exp) . toGroup


headersList :: ExperimentCourse -> [String]
headersList CompareSolutions = ["With demand", "No demand"]
headersList CompareInitialAbstractCovers = [
    "tS - QR", "tEnc - QR",
    "l", "r", "tr", "ty",
    "tS - B", "tr - B",
    "tS - Z", "tr - Z"
    ]
headersList TrackTypesAndTransitions = [
  "time", "encoding - s",
  "l", "r", "transitions", "types", "duplicate symbols", "status"
  ]

toRow :: ExperimentCourse -> (String, [ResultSummary]) -> [String]
toRow currentExp (name, rss) =
    map (fromMaybe "-") ([
      Just name,
      queryStr <$> mbPartial
      ] ++ (rowForExp currentExp))
  where
    findwhere name = find ((==) name . paramName)
    spaceList xs = intercalate ", " $ splitBy ',' $ show xs

    mbqr = findwhere expTyGarQ rss
    mbzero = findwhere expTyGar0 rss
    mbbaseline = findwhere expSypetClone rss
    mbPartial = find (\x -> (paramName x == expTyGarQ) && (envName x == "Partial"))  rss
    mbPartialNoDmd = find (\x -> (paramName x == expTyGarQNoDmd) && (envName x == "Partial"))  rss

    rowForExp :: ExperimentCourse -> [Maybe String]
    rowForExp CompareSolutions = let
        queryRefinementResults = (fromJust (results <$> mbPartial)) :: [Result]
        queryRefinementResultsNoDmd = (fromJust (results <$> mbPartialNoDmd)) :: [Result]
        toSolution = (either show id . resSolutionOrError) :: Result -> String
        myResults = [queryRefinementResults, queryRefinementResultsNoDmd]
      in
        map (\x -> Just $ unlines (map (mkOneLine . toSolution) (reverse x))) myResults

    rowForExp CompareInitialAbstractCovers = [
      (showFloat . resTFirstSoln) <$> (head . results) <$> mbqr,
      (showFloat . resTEncFirstSoln) <$> (head . results) <$> mbqr,
      (show . resLenFirstSoln) <$> (head . results) <$> mbqr,
      (show . resRefinementSteps) <$> (head . results) <$> mbqr,
      (show . resTransitions) <$> (head . results) <$> mbqr,
      (show . resTypes) <$> (head . results) <$> mbqr,

      (showFloat . resTFirstSoln) <$> (head . results) <$> mbbaseline,
      (show . resTransitions) <$> (head . results) <$> mbbaseline,
      (showFloat . resTFirstSoln) <$> (head . results) <$> mbzero,
      (show . resTransitions) <$> (head . results) <$> mbzero
      ]

    rowForExp TrackTypesAndTransitions = [
      (showFloat . resTFirstSoln) <$> (head . results) <$> mbqr,
      (showFloat . resTEncFirstSoln) <$> (head . results) <$> mbqr,
      (show . resLenFirstSoln) <$> (head . results) <$> mbqr,
      (show . resRefinementSteps) <$> (head . results) <$> mbqr,
      (spaceList . resTransitions . (head . results)) <$> mbqr,
      (spaceList . resTypes . (head . results)) <$> mbqr,
      (spaceList . resDuplicateSymbols . (head . results)) <$> mbqr,
      either show show <$> resSolutionOrError <$> (head . results) <$> mbqr
      ]