import * as Consts from "../constants/action-types";
import { combineReducers } from "redux";
import { initialSpecState, specReducer } from "./specReducer";
import { initialCandidateState, candidateReducer } from "./candidateReducer";
import { getDefaultFeatures } from "../utilities/featureManager";

const initialState = {
    spec: initialSpecState,
    candidates: initialCandidateState,
    features: getDefaultFeatures(),
    modal: {
        isOpen: false,
    }
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
        case Consts.MODAL_CLOSE:
            return {
                ...state,
                modal:{isOpen:false}
            };
        case Consts.MODAL_OPEN:
            return {
                ...state,
                modal:{isOpen:true}
            };
        default:
            return {
                ...state,
                ...restOfReducers(state, action)
            };
    }
}

export default rootReducer;
