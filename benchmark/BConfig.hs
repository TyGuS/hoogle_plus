module BConfig where

import Types.Generate
import Types.Experiments
import Types.Environment
import BTypes


defaultTimeout = 2 * 60 :: Int
defaultQueryFile = "benchmark/suites/allQueries.json"
defaultExperiment = TrackTypesAndTransitions

expTyGarQ = "TYGAR-Q"
expQueryRefinementHOF = "Query Refinement - HOF"
expSypetClone = "Sypet-Clone"
expTyGar0 = "TYGAR-0"
expTyGarQB = "TYGAR-QB"
expTyGar0B = "TYGAR-0B"
expNoGar = "NoGar"
expNoGar0 = "NoGar0"
expNoGarInc = "NoGar-Incremental"
expTyGar0BInc = "TyGar0B-Incremental"
expTyGarQInc = "TyGarQ-Incremental"
expTyGarQBInc = "TyGarQB-Incremental"
expNoGarTyGar0 = "NoGar with TyGar0's cover"
expNoGarTyGarQ = "NoGar with TyGarQ's cover"
expNoGarTyGar0B = "NoGar with TyGar0B's cover"
expNoGarTyGarQB = "NoGar with TyGarQB's cover"
expILP = "Integer Linear Programming"

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
searchParamsTyGar0BInc = searchParamsTyGar0B{_incrementalSolving=True}
searchParamsNoGarInc = searchParamsNoGar{_incrementalSolving=True}
searchParamsTyGarQBInc = searchParamsTyGarQB{_incrementalSolving=True}
searchParamsTyGarQInc = searchParamsTyGarQ{_incrementalSolving=True}

