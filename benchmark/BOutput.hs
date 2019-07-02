{-# LANGUAGE FlexibleInstances #-}
module BOutput where

import BTypes
import BConfig
import Synquid.Util
import Types.Experiments
import Types.Environment
import Types.Generate
import PetriNet.GHCChecker (toHaskellSolution)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.List
import Data.List.Extra
import Data.Bool (bool)
import Data.Maybe
import Text.Regex
import Text.Printf
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

toLaTeX :: [(String, [String])] -> String
toLaTeX table = let
  headers = map fst table
  body = map snd table
  bodyT = transpose body -- row-major order
  bodyTMultiLined = map (map mkMultiLined) bodyT
  bodyTWithIndex = zip [1..] bodyTMultiLined
  bodyTable = map toLatexLine bodyTWithIndex
  headerAlignment = intercalate "c" (replicate (length headers + 2) "|")
  headerLine = intercalate " & " ("Num" : headers) ++ "\\\\ \n\\hline"
  coredata = unlines (headerLine:bodyTable)
  in printf "\\begin{figure}\n\\resizebox{\\textwidth}{!}{ \\begin{tabular}{%s}\\hline\n%s\n\\hline \\end{tabular}} <yourcaptionhere> \\end{figure}" headerAlignment coredata
  where
    toLatexLine (idx, cells) = (replaceWithLatex $ intercalate " & " (show idx : cells)) ++ " \\\\"
    replaceWithLatex str = let
      replaceWith (match, sub) str = subRegex (mkRegex match) str sub
      replacements = [("->", "$\\rightarrow$"), ("=>", "$\\Rightarrow$")]
      in foldr replaceWith str replacements
    mkMultiLined str | length (lines str) == 0 = str
    mkMultiLined str | otherwise = let
      eachLine = lines str
      lineContent = intercalate " \\\\ " $ map replaceWithLatex eachLine
      tableWrapper = "\\begin{tabular}{@{}c@{}} %s \\end{tabular}"
      in printf tableWrapper lineContent

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
  outputSummary Table CompareSolutions = outputSummary TSV CompareSolutions -- Table is broken due to library bug here.
  outputSummary Table exp = toAsciiTable . (toTabling exp) . toGroup
  outputSummary TSV exp = toTSV . (toTabling exp) . toGroup
  outputSummary Latex exp = toLaTeX . (toTabling exp) . toGroup


headersList :: ExperimentCourse -> [String]
headersList CompareSolutions = [
  "T - first - Old", "T - all - New",
  "total - old", "total - new",
  "length - 1old", "length - 1new",
  "refinement steps - 1old", "refinement steps -1new",
  "transitions - 1old", "transitions - 1new",
  "Solutions - Old", "Solutions - New"
  ]
headersList CompareInitialAbstractCovers = [
    "tS - QR", "tEnc - QR",
    "l", "r", "tr", "ty",
    "tS - B", "tr - B",
    "tS - Z", "tr - Z"
    ]
headersList TrackTypesAndTransitions = [
  "time - no coalescing", "time - coalescing", "nc: length", "c: length",
  "nc: refinement steps", "c: refinement steps",
  "nc: transitions", "c: transitions",
  "duplicate symbols", "nc: status", "c: status"
  ]

toRow :: ExperimentCourse -> (String, [ResultSummary]) -> [String]
toRow currentExp (name, rss) =
    map (fromMaybe "-") ([
      Just name,
      queryStr <$> mbqr
      ] ++ (rowForExp currentExp))
  where
    findwhere name = find ((==) name . paramName)
    spaceList xs = intercalate ", " $ splitBy ',' $ show xs

    mbqr = findwhere expTyGarQ rss
    mbqrOld = find (\x -> (paramName x == expTyGarQ && envName x == (show ICFPPartial))) rss
    mbqrNew = find (\x -> (paramName x == expTyGarQ && envName x == (show POPL))) rss
    mbzero = findwhere expTyGar0 rss
    mbbaseline = findwhere expSypetClone rss
    mbExpNoDmd = find (\x -> (paramName x == expTyGarQNoDmd))  rss
    mbNoCoalescing = findwhere expTyGarQNoCoalesce rss

    rowForExp :: ExperimentCourse -> [Maybe String]
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
      (showFloat . resTFirstSoln) <$> (head . results) <$> mbNoCoalescing,
      (showFloat . resTFirstSoln) <$> (head . results) <$> mbqr,
      (show . resLenFirstSoln) <$> (head . results) <$> mbNoCoalescing,
      (show . resLenFirstSoln) <$> (head . results) <$> mbqr,
      (show . resRefinementSteps) <$> (head . results) <$> mbNoCoalescing,
      (show . resRefinementSteps) <$> (head . results) <$> mbqr,
      (show . resTransitions) <$> (head . results) <$> mbNoCoalescing,
      (show . resTransitions) <$> (head . results) <$> mbqr,
      (spaceList . resDuplicateSymbols . (head . results)) <$> mbqr,
      either show id <$> resSolutionOrError <$> (head . results) <$> mbNoCoalescing,
      either show id <$> resSolutionOrError <$> (head . results) <$> mbqr
      ]

    rowForExp CompareSolutions = let
        -- come in reverse order, so we must flip it.
        queryRefinementResults = reverse (fromJust (results <$> mbqrNew)) :: [Result]
        queryRefinementResultsOld = reverse (fromJust (results <$> mbbaseline)) :: [Result]
        toSolution = (either show id . resSolutionOrError) :: Result -> String
        timeToAll = sum $ map resTFirstSoln queryRefinementResults
        timeToAllOld = sum $ map resTFirstSoln queryRefinementResultsOld
      in
        [
          (showFloat . resTFirstSoln) <$> listToMaybe queryRefinementResultsOld, -- First
          (showFloat . resTFirstSoln) <$> listToMaybe queryRefinementResults, -- First
          bool Nothing (Just (showFloat timeToAllOld)) (timeToAllOld /= 0), -- All
          bool Nothing (Just (showFloat timeToAll)) (timeToAll /= 0), -- All

          (show . resLenFirstSoln) <$> (head . results) <$> mbbaseline,
          (show . resLenFirstSoln) <$> (head . results) <$> mbqrNew,
          (show . resRefinementSteps) <$> (head . results) <$> mbbaseline,
          (show . resRefinementSteps) <$> (head . results) <$> mbqrNew,
          (show . resTransitions) <$> (head . results) <$> mbbaseline,
          (show . resTransitions) <$> (head . results) <$> mbqrNew,

          Just $ unlines (map (mkOneLine . toSolution) queryRefinementResultsOld),
          Just $ unlines (map (mkOneLine . toSolution) queryRefinementResults)
        ]
