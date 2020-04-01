import * as Consts from "../constants/action-types";
import { LOADING, DONE, ERROR } from "../constants/fetch-states";
import {v4 as uuidv4, v4} from "uuid";
import { usageToId, inputsToId } from "../utilities/args";

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
         *     errorMessage: null || str
         *     examples: [
         *         {
         *             id: usageToId(["x", "2", "xx"]),
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

const usageToExample = (usage) => {
    return {
        id: usageToId(usage),
        usage: usage,
        isLoading: false,
    };
}

const toExample = (inputs, output) => {
    return {
        id: inputsToId(inputs),
        inputs,
        output,
        usage: inputs.concat(output),
        isLoading: false,
    };
}

export function candidateReducer(state = initialCandidateState, action){
    switch(action.type) {
        case Consts.CLEAR_RESULTS:
            return {...state, results:[]};
        case Consts.SET_SEARCH_STATUS:
            return {
                ...state,
                isFetching: action.payload.status === LOADING,
            };
        case Consts.ADD_CANDIDATE:
            const newResults = action.payload.result.map((candidate) => {
                const currentPrograms = state.results.map((existCandidate) => existCandidate.code);
                const idx = state.results.indexOf((code) => code == candidate.code);
                if (idx != -1)  { // this is an old candidate, but new examples come
                    const newExamples = candidate.examples.map(toExample);
                    state.results[idx].examples.concat(newExamples);
                } else { // this is a new candidate
                    const newResult = {
                        candidateId: v4(),
                        code: candidate.code,
                        docs: action.payload.docs,
                        examplesStatus: DONE,
                        examples: candidate.examples.map(({inputs, output}) => {
                            return {
                                id: inputsToId(inputs),
                                inputs,
                                output,
                                usage: inputs.concat(output),
                                isLoading: false,
                            }}),
                    };
                    state.results.concat(newResult);
                }
            });
            return {
                ...state,
                results: newResults,
                queryId: action.payload.queryId,
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
                return {...result, examples: updatedExamples};
            });
            return {...state, results:resultsWithNewUsage};

        case Consts.STOP_SEARCH:
            return {
                ...state,
                queryId: null
            };
        default:
            return state;
    }
};
