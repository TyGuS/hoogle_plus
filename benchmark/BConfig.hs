module BConfig where

import Types.Generate
import Types.Experiments
import Types.Environment
import BTypes


defaultTimeout = 2 * 60 :: Int
defaultQueryFile = "benchmark/suites/allQueries.json"
defaultExperiment = TrackTypesAndTransitions

searchParamsTyGarQ = defaultSearchParams
searchParamsHOF = defaultSearchParams{_useHO=True}
searchParamsSypetClone = defaultSearchParams{_refineStrategy=SypetClone}
searchParamsTyGar0 = defaultSearchParams{_refineStrategy=TyGar0}
searchParamsTyGarQB = defaultSearchParams{_stopRefine=True,_threshold=5}
searchParamsTyGar0B = searchParamsTyGarQB{_refineStrategy=TyGar0}
searchParamsNoGar = defaultSearchParams{_refineStrategy=NoGar}
searchParamsNoGar0 = defaultSearchParams{_refineStrategy=NoGar0}
searchParamsNoGarTyGar0 = defaultSearchParams{_refineStrategy=NoGarTyGar0}
searchParamsNoGarTyGarQ = defaultSearchParams{_refineStrategy=NoGarTyGarQ}
searchParamsNoGarTyGar0B = defaultSearchParams{_refineStrategy=NoGarTyGar0B}
searchParamsNoGarTyGarQB = defaultSearchParams{_refineStrategy=NoGarTyGarQB}
searchParamsILP = defaultSearchParams
