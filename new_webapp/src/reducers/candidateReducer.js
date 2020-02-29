import * as Consts from "../constants/action-types";
import { LOADING, DONE } from "../constants/fetch-states";
import {v4 as uuidv4} from "uuid";

export const initialCandidateState = {
    isFetching: false,
    results: [
        {
            candidateId: "cand1",
            code: "\\arg0 arg1-> replicate arg0 arg1",
            examplesLoading: false,
            examples: [
                {
                    id: "0",
                    usage: ["x", "2", "xx"],
                    isLoading: false,
                }, {
                    id: "2",
                    usage: ["x", "0", ""],
                    isLoading: false,
                    error: "errorstring",
                }
            ]
        },
        {
            candidateId: "cand2",
            code: "\\arg0 arg1-> replicate2 arg0 arg1",
            examplesLoading: false,
            examples: [
                {
                    id: "0",
                    usage: ["x", "2", "xx"],
                    isLoading: false,
                }
            ]
        }
    ]
};

const usageToExample = (usage) => {
    return {
        id: uuidv4(),
        usage: usage,
        isLoading: false,
    };
}

export function candidateReducer(state = initialCandidateState, action){
    switch(action.type) {
        case Consts.ADD_CANDIDATE:
            return {
                ...state,
                results: state.results.concat(action.payload)
            }
        case Consts.SET_SEARCH_TYPE:
            return {
                results: [],
                isFetching: true,
            }
        case Consts.FETCH_MORE_CANDIDATE_USAGES:
            const {candidateId:fCandidateId, status} = action.payload;
            switch (status) {
                case LOADING:
                    const newLoadingResults = state.results.map((result) => {
                        if (result.candidateId === fCandidateId) {
                            return {
                                ...result,
                                examplesLoading: true,
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
                                examplesLoading: false,
                                examples: result.examples.concat(newExamples)
                            };
                        }
                        return result;
                    });
                    return {...state, results: newExampleResults};
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
                                return {
                                    id: example.id,
                                    usage: action.payload.args.concat(null),
                                    isLoading: true,
                                };
                            }
                            if (action.payload.result) {
                                let updatedUsage = Array.from(example.usage);
                                updatedUsage[updatedUsage.length - 1] = action.payload.result;
                                return {
                                    id: example.id,
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

        default:
            return state;
    }
};