{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module BPlot where

import BTypes
import BConfig
import Synquid.Util
import Types.Experiments
import Types.Environment
import Types.Generate
import BOutput

import Graphics.EasyPlot
import qualified Graphics.EasyPlot as EP
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.List
import Data.List.Extra
import Data.Bool (bool)
import Data.Maybe
import Text.Printf
import Data.Either
import Data.Tuple
import Text.Printf
import System.FilePath.Posix
import Data.Char (toLower)

deriving instance Eq Color

instance Enum Color where
    fromEnum = fromJust . flip lookup table
    toEnum = fromJust . flip lookup (map swap table)
table = zip [
    Red, Blue, Green, Yellow, Orange, Magenta,
    Cyan, DarkRed, DarkBlue, DarkGreen, DarkYellow,
    DarkOrange, DarkMagenta, DarkCyan, LightRed,
    LightBlue, LightGreen, LightMagenta, Violet,
    White, Brown, Grey, DarkGrey, Black] [0..]

-- X axis: number of benchmarks
-- Y axis: time solved
-- Sorts benchmarks per setup and plots as lines.
mkPlot :: Maybe FilePath -> ExperimentSetup -> [ResultSummary] -> IO ()
mkPlot outputFile setup results = do
    let options = [Title "test title", Color Red, Style Lines]
    let groupedResults = groupMapBy (\rs -> (envName rs, paramName rs)) results
    let plotDataWithTitle = getPlotData setup groupedResults
    let plotOptions = getPlotOptions plotDataWithTitle
    let plotData = map snd plotDataWithTitle
    let options2d = []
    let plots = zipWith (\pd opts -> Data2D opts options2d pd) plotData plotOptions
    let outputFormat = getOutputFormat outputFile
    plot' [Debug] outputFormat plots
    return ()

getOutputFormat :: Maybe FilePath -> TerminalType
getOutputFormat Nothing = PDF "plot.pdf"
getOutputFormat (Just file) = case (map toLower $ takeExtension file) of
    ".pdf" -> PDF file
    ".tex" -> EP.Latex file
    ".gif" -> GIF file
    ".svg" -> SVG file
    ".png" -> PNG file
    _ -> PDF file

getPlotOptions :: [(String, a)] -> [[Option]]
getPlotOptions = foldr addPlot []
    where
        addPlot (title, _ ) xs = let
            nextColor = getNextColor xs
            in
                [Color nextColor, Style Lines, Title title]:xs
        getNextColor [] = Red
        getNextColor (opts:_) = case find isColor opts of
            Nothing -> Red
            Just (Color c)-> succ c
        isColor (Color _) = True
        isColor _ = False

getPlotData :: (Fractional w) => ExperimentSetup
            -> Map (String, String) [ResultSummary] -> [(String, [(w, Double)])]
getPlotData setup groupedResults = let
    groupedValidResults =
        filter (\(_, rs) -> not $ null rs) $
        map didComplete $
        Map.toList groupedResults
    sortedResults = map orderResults groupedValidResults
    in
        map toOutput sortedResults
    where
        orderResults (l,r) = (l, sortOn (resTFirstSoln . head) r)
        toPlot x = resTFirstSoln $ head x
        toOutput ((envName, paramName), results) = let
            keyName = printf "%s-%s" envName paramName
            plotResults = map toPlot results
            in
                (keyName, zip (map fromRational [1..]) $ plotResults)
        didComplete (k, rss) = (k, map results $ filter didComplete' rss)
        didComplete' ResultSummary{results} =
            length results > 0 &&
            resTFirstSoln (head results) > 0 &&
            not (isLeft (resSolutionOrError (head results)))