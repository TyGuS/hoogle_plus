import * as Consts from "../constants/action-types";
import { combineReducers } from "redux";
import { initialSpecState, specReducer } from "./specReducer";
import { initialCandidateState, candidateReducer } from "./candidateReducer";

const initialState = {
    spec: initialSpecState,
    candidates: initialCandidateState,
};

// This allows us to silo state in these separate areas and break the code
// out into different more self contained files.
const restOfReducers = combineReducers({
    candidates: candidateReducer,
    spec: specReducer
})

const rootReducer = (state = initialState, action) => {
    switch(action.type) {
        //Catch any modifies of numArgs here.
        default:
            return {
                ...state,
                ...restOfReducers(state, action)
            }
    }
}

export default rootReducer;