{-# LANGUAGE NamedFieldPuns #-}

module BPlot where

import BTypes
import BConfig
import Synquid.Util
import Types.Experiments
import Types.Environment
import Types.Generate
import BOutput

import qualified Graphics.Gnuplot.Simple as GP
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.PostScript as PS
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

-- X axis: number of benchmarks
-- Y axis: time solved
-- Sorts benchmarks per setup and plots as lines.
mkPlot :: Maybe FilePath -> ExperimentSetup -> [ResultSummary] -> IO ()
mkPlot outputFile setup results = do
    let groupedResults = groupMapBy (\rs -> (envName rs, paramName rs)) results
    let plotDataWithTitle = getPlotData setup groupedResults

    let queryCount = maximum (map length $ Map.elems groupedResults)
    let plotOptions = [
          YRange (1, fromIntegral queryCount),
          YLabel "Benchmarks Passed",
          XLabel "Seconds"
          ] ++ (getOutputFormat outputFile)
    GP.plotListsStyle plotOptions plotDataWithTitle
    return ()

getOutputFormat :: Maybe FilePath -> [Attribute]
getOutputFormat Nothing = getOutputFormat (Just "output.ps")
getOutputFormat (Just file) = case (map toLower $ takeExtension file) of
    ".ps" -> [terminal (PS.color $ PS.cons file)]
    ".svg" -> [terminal (SVG.cons file)]
    _ -> getOutputFormat Nothing

getDatafieldToPlot :: [Result] -> Double
getDatafieldToPlot x = resTFirstSoln $ head x

getPlotData :: ExperimentSetup
            -> Map (String, String) [ResultSummary] -> [(PlotStyle, [(Double, Int)])]
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

        addStyle name = defaultStyle {lineSpec = CustomStyle [LineTitle name]}

        toOutput ((envName, paramName), results) = let
            keyName = (printf "%s-%s" envName paramName)::String
            style = addStyle keyName
            plotResults = map getDatafieldToPlot results
            in
                (style, zip plotResults ([1..]))

        didComplete (k, rss) = (k, map results $ filter didComplete' rss)
        didComplete' ResultSummary{results} =
            length results > 0 &&
            resTFirstSoln (head results) > 0 &&
            not (isLeft (resSolutionOrError (head results)))