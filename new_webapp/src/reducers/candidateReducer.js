import * as Consts from "../constants/action-types";

export const initialCandidateState = {
    isFetching: false,
    results: [
        {
            code: "\\arg0 arg1-> replicate arg0 arg1",
            examples: [
                {
                    id: "1",
                    usage: ["x", "2", "xx"],
                }, {
                    id: "2",
                    usage: ["x", "0", ""],
                }
            ]
        },
        {
            code: "\\arg0 arg1-> replicate2 arg0 arg1",
            examples: [
                {
                    id: "1",
                    usage: ["y", "3", "yyy"],
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
        default:
            return state;
    }
};