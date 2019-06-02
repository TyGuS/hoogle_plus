module BConfig where

import Types.Generate
import Types.Experiments
import Types.Environment
import BTypes


defaultTimeout = 2 * 60 :: Int
defaultQueryFile = "benchmark/suites/test.json"
defaultExperiment = TrackTypesAndTransitions

searchParams = defaultSearchParams
searchParamsHOF = defaultSearchParams{_useHO=True}
searchParamsBaseline = defaultSearchParams{_refineStrategy=NoRefine}
searchParamsZeroStart = defaultSearchParams{_refineStrategy=AbstractRefinement}
