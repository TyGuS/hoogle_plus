import * as Consts from "../constants/action-types";
import { combineReducers } from "redux";
import { initialFactState, factReducer } from "./factReducer";
import { initialCandidateState, candidateReducer } from "./candidateReducer";

const initialState = {
    numArgs: 2,
    searchingStatus: Consts.DONE,
    facts: initialFactState,
    candidates: initialCandidateState,
};

// This allows us to silo state in these separate areas and break the code
// out into different more self contained files.
const restOfReducers = combineReducers({
    candidates: candidateReducer,
    facts: factReducer
})

const rootReducer = (state = initialState, action) => {
    switch(action.type) {
        //Catch any modifies of numArgs or searchStatus here.
        default:
            return {
                ...state,
                ...restOfReducers(state, action)
            }
    }
}

export default rootReducer;