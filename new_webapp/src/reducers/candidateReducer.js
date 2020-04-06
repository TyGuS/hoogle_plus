import * as Consts from "../constants/action-types";
import { LOADING, DONE, ERROR } from "../constants/fetch-states";
import {v4 as uuidv4, v4} from "uuid";
import { usageToId, inputsToId } from "../utilities/args";
import { _ } from "underscore";
import { defaultExamplesShown, defaultExamplesShownIncrement } from "../utilities/featureManager";

export const initialCandidateState = {
    isFetching: false,
    queryId: null,
    results: [
        /** {
         *     candidateId: "cand1",
         *     code: "\\arg0 arg1 -> replicate arg0 arg1",
         *     examplesStatus: DONE,
         *     docs: [
         *      { doc: str,
         *        name: str,
         *        signature: str
         *      }
         *     ]
         *     errorMessage: null || str,
         *     examplesShown: 3,
         *     examples: [
         *         {
         *             id: inputsToId(["x", "2", "xx"]),
         *             inputs: ["x", "2"],
         *             output: "xx",
         *             isLoading: false,
         *         }, {
         *             id: "2",
         *             inputs: ["x", "0"],
         *             output: "0",
         *             isLoading: false,
         *             error: "errorstring",
         *         }
         *     ]
         * },
         * {
         *     candidateId: "cand2",
         *     code: "\\arg0 arg1-> replicate2 arg0 arg1",
         *     examplesStatus: ERROR,
         *     examplesShown: 3,
         *     examples: [
         *         {
         *             id: usageToId(["x", "2", "xx"]),
         *             inputs: ["x", "2"],
         *             output: "xx",
         *             isLoading: false,
         *         }
         *     ]
         * }
        **/
    ]
};

const toExample = ({inputs, output}) => {
    return {
        id: inputsToId(inputs),
        inputs,
        output,
        isLoading: false,
    };
}

export function candidateReducer(state = initialCandidateState, action){
    switch(action.type) {
        case Consts.FILTER_RESULTS:
            const examplesMustPass = action.payload.examples;
            if (!examplesMustPass || examplesMustPass.length === 0) {
                return {...state, results:[], spec:{}};
            }
            return {
                ...state,
                results: state.results.filter((candidate) => {
                    return _.any(candidate.examples, (ex) => {
                        return _.all(examplesMustPass, (specEx) => {
                            return (_.isEqual(specEx.inputs,ex.inputs) && (specEx.output === ex.output));
                        });
                    });
                }),
            };
        case Consts.SET_SEARCH_STATUS:
            return {
                ...state,
                isFetching: action.payload.status === LOADING,
            };
        case Consts.ADD_CANDIDATE:
            const candidates = action.payload.candidates;
            const newResults = candidates.reduce((accumResults, candidate) => {
                const currentPrograms = accumResults.map((existCandidate) => existCandidate.code);
                const idx = currentPrograms.indexOf(candidate.code);
                if (idx !== -1)  { // this is an old candidate, but new examples come
                    const newExamples = candidate.examples.map(toExample);
                    const targetExample = accumResults[idx].examples;
                    const targetIds = targetExample.map((ex) => ex.id);
                    const uniqueExamples = newExamples.filter((ex) => targetIds.indexOf(ex.id) < 0);
                    accumResults[idx].examples = targetExample.concat(uniqueExamples);
                    return accumResults;
                } else { // this is a new candidate
                    const newResult = {
                        candidateId: v4(),
                        code: candidate.code,
                        docs: action.payload.docs,
                        examplesStatus: DONE,
                        examplesShown: defaultExamplesShown,
                        examples: candidate.examples.map(toExample),
                    };
                    return accumResults.concat(newResult);
                }
            }, state.results.slice());
            return {
                ...state,
                results: newResults,
                queryId: action.payload.id,
            };
        case Consts.SET_SEARCH_TYPE:
            return {
                results: [],
                isFetching: false,
            };
        case Consts.FETCH_MORE_CANDIDATE_USAGES:
            const {candidateId:fCandidateId, status} = action.payload;
            switch (status) {
                case LOADING:
                    const newLoadingResults = state.results.map((result) => {
                        if (result.candidateId === fCandidateId) {
                            return {
                                ...result,
                                examplesStatus: LOADING,
                            };
                        }
                        return result;
                    });
                    return {...state, results: newLoadingResults};
                case DONE:
                    const newExampleResults = state.results.map((result) => {
                        if (result.candidateId === fCandidateId) {
                            const newExamples = action.payload.result.map(example => {
                                return {
                                    ...example,
                                    id: v4(), // Attach an ID to the new example
                                };
                            });
                            return {
                                ...result,
                                examplesStatus: DONE,
                                examples: result.examples.concat(newExamples)
                            };
                        }
                        return result;
                    });
                    return {...state, results: newExampleResults};
                case ERROR:
                    const newFailedResults = state.results.map((result) => {
                        if (result.candidateId === fCandidateId) {
                            return {
                                ...result,
                                examplesStatus: ERROR,
                                errorMessage: action.payload.message,
                            }
                        }
                        return result;
                    });
                    return {...state, results: newFailedResults};
            }
            return {
                ...state,
            };
        // Update one particular usage.
        case Consts.UPDATE_CANDIDATE_USAGE:
            const {candidateId:uCandidateId, usageId} = action.payload;
            const updatedResults = state.results.map(result => {
                if(result.candidateId === uCandidateId) {
                    const updatedExamples = result.examples.map(example => {
                        if (example.id === usageId) {
                            if (action.payload.inputs) {
                                return {
                                    id: usageId,
                                    inputs: action.payload.inputs,
                                    isLoading: true,
                                };
                            }
                            if (action.payload.output) {
                                const updatedId = inputsToId(example.inputs)
                                return {
                                    id: updatedId,
                                    inputs: example.inputs,
                                    output: action.payload.output,
                                    isLoading: false,
                                };
                            }
                            if (action.payload.error) {
                                return {...example,
                                    isLoading: false,
                                    error: action.payload.error
                                };
                            }
                        }
                        return example;
                    })
                    return {...result, examples: updatedExamples};
                }
                return result;
            })
            return {...state, results:updatedResults};

        // action.payload : {candidateId, usageId, inputs}
        case Consts.ADD_CANDIDATE_USAGE:
            const {candidateId:addToId, usageId:withUsageId, inputs:newInputs} = action.payload;
            const resultsWithNewUsage = state.results.map(result => {
                if (result.candidateId !== addToId) {
                    return result;
                }
                const updatedExamples = result.examples.concat({
                    id: withUsageId,
                    isLoading: true,
                    inputs: newInputs,
                    output: "??",
                })
                return {
                    ...result,
                    examplesShown: (result.examplesShown || defaultExamplesShown) + 1,
                    examples: updatedExamples
                };
            });
            return {...state, results:resultsWithNewUsage};

        case Consts.SHOW_MORE_USAGES:
            const {candidateId:cid, newValue} = action.payload;
            const resultsWithNewUsage_smu = state.results.map(result => {
                if (result.candidateId !== cid) {
                    return result;
                }
                return {
                    ...result,
                    examplesShown: newValue,
                };
            });
            return {...state, results:resultsWithNewUsage_smu};

        case Consts.STOP_SEARCH:
            return {
                ...state,
                queryId: null
            };
        default:
            return state;
    }
};
