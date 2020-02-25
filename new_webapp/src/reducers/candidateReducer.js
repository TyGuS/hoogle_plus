import * as Consts from "../constants/action-types";

export const initialCandidateState = {
    isFetching: false,
    results: [
        {
            candidateId: "cand1",
            code: "\\arg0 arg1-> replicate arg0 arg1",
            examples: [
                {
                    id: "1",
                    usage: ["x", "2", "xx"],
                    isLoading: false,
                }, {
                    id: "2",
                    usage: ["x", "0", ""],
                    isLoading: false,
                }
            ]
        },
        {
            candidateId: "cand2",
            code: "\\arg0 arg1-> replicate2 arg0 arg1",
            examples: [
                {
                    id: "1",
                    usage: ["y", "3", "yyy"],
                    isLoading: false,
                }
            ]
        }
    ]
};

export function candidateReducer(state = initialCandidateState, action){
    switch(action.type) {
        case Consts.ADD_CANDIDATE:
            return {
                ...state,
                results: state.results.concat(action.payload)
            }
        case Consts.SET_NEW_SEARCH:
            return {
                results: [],
                isFetching: true,
            }
        case Consts.UPDATE_CANDIDATE_USAGE:
            const {candidateId, usageId} = action.payload;
            const updatedResults = state.results.map(result => {
                if(result.candidateId === candidateId) {
                    const updatedExamples = result.examples.map(example => {
                        if (example.id === usageId) {
                            if (action.payload.newArgs) {
                                return {...example,
                                    usage: action.payload.newArgs.concat(null),
                                    isLoading: true,
                                };
                            }
                            if (action.payload.result) {
                                let updatedUsage = Array.from(example.usage);
                                updatedUsage[updatedUsage.length - 1] = action.payload.result;
                                return {...example,
                                    usage: updatedUsage,
                                    isLoading: false,
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