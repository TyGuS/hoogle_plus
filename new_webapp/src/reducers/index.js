import { ADD_CANDIDATE } from "../constants/action-types";

const initialState = {
    candidates: [
        {
            code: "\\arg0 arg1-> replicate arg0 arg1",
            examples: [
                ["x", "2", "xx"]
            ]
        }
    ]
};

function rootReducer(state = initialState, action){
    if (action.type === ADD_CANDIDATE) {
        return {
            ...state,
            candidates: state.candidates.concat(action.payload)
        };
    }
    return state
};

export default rootReducer;