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
searchParamsSypetClone = defaultSearchParams{_useRefine=SypetClone}
searchParamsTyGar0 = defaultSearchParams{_useRefine=TyGar0}
searchParamsTyGarQB = defaultSearchParams{_earlyCut=True,_stopThresh=5}
searchParamsTyGar0B = searchParamsTyGarQB{_useRefine=TyGar0}
searchParamsNoGar = defaultSearchParams{_useRefine=NoGar}
searchParamsNoGar0 = defaultSearchParams{_useRefine=NoGar0}
searchParamsNoGarTyGar0 = defaultSearchParams{_useRefine=NoGarTyGar0}
searchParamsNoGarTyGarQ = defaultSearchParams{_useRefine=NoGarTyGarQ}
searchParamsNoGarTyGar0B = defaultSearchParams{_useRefine=NoGarTyGar0B}
searchParamsNoGarTyGarQB = defaultSearchParams{_useRefine=NoGarTyGarQB}
searchParamsILP = defaultSearchParams
searchParamsTyGars = map (\t -> searchParamsTyGar0{_earlyCut=True,_stopThresh=t}) [0..10]
