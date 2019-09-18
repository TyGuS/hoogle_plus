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
toGroup rss = groupMapBy queryName rss

groupMapBy :: Ord k => (ResultSummary -> k) -> [ResultSummary] -> Map k [ResultSummary]
groupMapBy f results = foldr updateMap Map.empty results
    where
      updateMap result currentMap = Map.insertWith (++) (f result) [result] currentMap

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
        [show $ length $ Map.elems $ _symbols env],
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
headersList POPLQuality =
  let getRows x = map (\y -> y ++ (" - " ++ x)) ["T - first", "T - all", "length", "ref", "Transitions", "Solutions"]
  in concat (transpose (map getRows ["dmd", "nodmd", "norel"]))

headersList CompareEnvironments = [
  "T - first - Old", "T - all - New",
  "total - old", "total - new",
  "length - 1old", "length - 1new",
  "refinement steps - 1old", "refinement steps -1new",
  "transitions - 1old", "transitions - 1new",
  "Solutions - Old", "Solutions - New"
  ]
headersList CoalescingStrategies = [
  "T-None", "T-Naive", "T-Least", "T-Most",
  "Ref-None", "Ref-Naive", "Ref-Least", "Ref-Most",
  "Soln-None", "Soln-Naive", "Soln-Least", "Soln-Most"
  ]
headersList CompareInitialAbstractCovers = [
    "T-Q", "T-0", "T-Sypet",
    "Ref-Q", "Ref-0", "Ref-Sypet",
    "Transitions-Q", "Transitions-0", "Transitions-Sypet",
    "Types-Q", "Types-0", "Types-Sypet",
    "Solution-Q", "Solution-0", "Solution-Sypet"
  ]
headersList TrackTypesAndTransitions = [
  "time - no coalescing", "time - coalescing", "nc: length", "c: length",
  "nc: refinement steps", "c: refinement steps",
  "nc: transitions", "c: transitions",
  "duplicate symbols", "nc: status", "c: status"
  ]
headersList err = error $ "Missing case for: " ++ show err

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
    mbExpNoDmd = find (\x -> (paramName x == expTyGarQBNoDmd))  rss
    mbNoCoalescing = findwhere expTyGarQNoCoalesce rss

    rowForExp :: ExperimentCourse -> [Maybe String]
    rowForExp POPLQuality = let
      mbqr = findwhere expTyGarQB rss
      mbqrNoDmd = findwhere expTyGarQBNoDmd rss
      mbqrNoRel = findwhere expTyGarQBNoRel rss
      toSolution = (either show id . resSolutionOrError) :: Result -> String
      qrr = reverse (fromJust (results <$> mbqr)) :: [Result]
      qrrNoDmd = reverse (fromJust (results <$> mbqrNoDmd)) :: [Result]
      qrrNoRel = reverse (fromJust (results <$> mbqrNoRel)) :: [Result]
      timeToAll x = sum $ map resTFirstSoln x
      getRows x = [
        (showFloat . resTFirstSoln) <$> listToMaybe x, -- First
        bool Nothing (Just (showFloat (timeToAll x))) ((timeToAll x) /= 0), -- All
        (show . resLenFirstSoln) <$> listToMaybe x,
        (show . resRefinementSteps) <$> listToMaybe x,
        (show . resTransitions) <$> listToMaybe x,
        Just $ unlines (map (mkOneLine . toSolution) x)
        ]
      in
        concat (transpose (map getRows [qrr, qrrNoDmd, qrrNoRel]))

    rowForExp CompareInitialAbstractCovers = let
      tygar = fromJust (results <$> findwhere expTyGarQ rss)
      tygar0 = fromJust (results <$> findwhere expTyGar0 rss)
      sypet = fromJust (results <$> findwhere expSypetClone rss)
      getRows x = [
        (showFloat . resTFirstSoln) <$> listToMaybe x,
        (show . resRefinementSteps) <$> listToMaybe x,
        (show . resTransitions) <$> listToMaybe x,
        (show . resTypes) <$> listToMaybe x,
        either show id <$> resSolutionOrError <$> listToMaybe x
        ]
      in
        concat (transpose (map getRows [tygar, tygar0, sypet]))


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

    rowForExp CoalescingStrategies = let
      nc = findwhere expTyGarQNoCoalesce rss
      naive = findwhere expTyGarQCoalesceFirst rss
      least = findwhere expTyGarQCoalesceLeast rss
      most = findwhere expTyGarQCoalesceMost rss
      in
        [
          (showFloat . resTFirstSoln) <$> (head . results) <$> nc,
          (showFloat . resTFirstSoln) <$> (head . results) <$> naive,
          (showFloat . resTFirstSoln) <$> (head . results) <$> least,
          (showFloat . resTFirstSoln) <$> (head . results) <$> most,
          (show . resRefinementSteps) <$> (head . results) <$> nc,
          (show . resRefinementSteps) <$> (head . results) <$> naive,
          (show . resRefinementSteps) <$> (head . results) <$> least,
          (show . resRefinementSteps) <$> (head . results) <$> most,
          either show id <$> resSolutionOrError <$> (head . results) <$> nc,
          either show id <$> resSolutionOrError <$> (head . results) <$> naive,
          either show id <$> resSolutionOrError <$> (head . results) <$> least,
          either show id <$> resSolutionOrError <$> (head . results) <$> most
        ]

    rowForExp CompareEnvironments = let
        -- come in reverse order, so we must flip it.
        queryRefinementResults = reverse (fromJust (results <$> mbqrNew)) :: [Result]
        queryRefinementResultsOld = reverse (fromJust (results <$> mbqrOld)) :: [Result]
        toSolution = (either show id . resSolutionOrError) :: Result -> String
        timeToAll = sum $ map resTFirstSoln queryRefinementResults
        timeToAllOld = sum $ map resTFirstSoln queryRefinementResultsOld
      in
        [
          (showFloat . resTFirstSoln) <$> listToMaybe queryRefinementResultsOld, -- First
          (showFloat . resTFirstSoln) <$> listToMaybe queryRefinementResults, -- First
          bool Nothing (Just (showFloat timeToAllOld)) (timeToAllOld /= 0), -- All
          bool Nothing (Just (showFloat timeToAll)) (timeToAll /= 0), -- All

          (show . resLenFirstSoln) <$> (head . results) <$> mbqrOld,
          (show . resLenFirstSoln) <$> (head . results) <$> mbqrNew,
          (show . resRefinementSteps) <$> (head . results) <$> mbqrOld,
          (show . resRefinementSteps) <$> (head . results) <$> mbqrNew,
          (show . resTransitions) <$> (head . results) <$> mbqrOld,
          (show . resTransitions) <$> (head . results) <$> mbqrNew,

          Just $ unlines (map (mkOneLine . toSolution) queryRefinementResultsOld),
          Just $ unlines (map (mkOneLine . toSolution) queryRefinementResults)
        ]
