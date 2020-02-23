import * as Consts from "../constants/action-types";
import { combineReducers } from "redux";

const initialState = {
    arguments: 2,
    searchingStatus: Consts.DONE,
    facts: {
        editingCells: [],
        rows: [["x", "2", "xx"]]
    },
    candidates: [
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
    ]
};

const mkDoubleList = (listList) => {
    return listList.map(element => {
        return [
            element.arg0,
            element.arg1,
            element.arg2
        ]
    });
}

function rootReducer(state = initialState, action){
    switch(action.type) {
        case Consts.ADD_CANDIDATE:
            return {
                ...state,
                candidates: state.candidates.concat(action.payload),
            };
        case Consts.ADD_FACT:
            return {
                ...state,
                facts: {
                    ...state.facts,
                    rows: state.facts.rows.concat([action.payload]),
                }
            };
        case Consts.SET_FACTS:
            return {
                ...state,
                facts: {
                    ...state.facts,
                    rows: mkDoubleList(action.payload),
                }
            };
        case Consts.SET_EDITING_CELLS:
            return {
                ...state,
                facts: {
                    ...state.facts,
                    editingCells: action.payload,
                }
            };
        default:
            return state;
    }
};

// const rootReducer = combineReducers({
//     defaultReducer,
//     // factReducer
// });

export default rootReducer;