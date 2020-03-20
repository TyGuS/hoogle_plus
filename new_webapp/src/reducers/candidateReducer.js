import * as Consts from "../constants/action-types";
import { LOADING, DONE, ERROR } from "../constants/fetch-states";
import {v4 as uuidv4} from "uuid";
import { usageToId } from "../utilities/args";

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
         *             usage: ["x", "2", "xx"],
         *             isLoading: false,
         *         }, {
         *             id: "2",
         *             usage: ["x", "0", ""],
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
         *             usage: ["x", "2", "xx"],
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
            return {
                ...state,
                results: state.results.concat(action.payload.result),
                queryId: action.payload.queryId,
            }
        case Consts.SET_SEARCH_TYPE:
            return {
                results: [],
                isFetching: false,
                }
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
                            const newUsages = action.payload.result;
                            const newExamples = newUsages.map(usageToExample);
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
        case Consts.UPDATE_CANDIDATE_USAGE:
            const {candidateId:uCandidateId, usageId} = action.payload;
            const updatedResults = state.results.map(result => {
                if(result.candidateId === uCandidateId) {
                    const updatedExamples = result.examples.map(example => {
                        if (example.id === usageId) {
                            if (action.payload.args) {
                                let inProgressUsage = action.payload.args.concat(null);
                                return {
                                    id: usageId,
                                    usage: inProgressUsage,
                                    isLoading: true,
                                };
                            }
                            if (action.payload.result) {
                                let updatedUsage = Array.from(example.usage);
                                updatedUsage[updatedUsage.length - 1] = action.payload.result;
                                return {
                                    id: usageToId(updatedUsage),
                                    usage: updatedUsage,
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

        case Consts.STOP_SEARCH:
            return {
                ...state,
                queryId: null
            };
        default:
            return state;
    }
};
