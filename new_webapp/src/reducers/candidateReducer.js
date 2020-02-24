import * as Consts from "../constants/action-types";

export const initialCandidateState = [
    {
        code: "\\arg0 arg1-> replicate arg0 arg1",
        examples: [
            ["x", "2", "xx"],
            ["x", "0", ""],
        ]
    },
    {
        code: "\\arg0 arg1-> replicate2 arg0 arg1",
        examples: [
            ["y", "3", "yyy"]
        ]
    }
];

export function candidateReducer(state = initialCandidateState, action){
    switch(action.type) {
        case Consts.ADD_CANDIDATE:
            return state.candidates.concat(action.payload)
        default:
            return state;
    }
};