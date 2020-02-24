import * as Consts from "../constants/action-types";

export const initialFactState = {
    editingCells: [],
    rows: [["x", "2", "xx"]]
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

export function factReducer(state = initialFactState, action) {
    switch(action.type) {
        case Consts.ADD_FACT:
            return {
                ...state,
                rows: state.rows.concat([action.payload]),
            };
        case Consts.SET_FACTS:
            return {
                ...state,
                rows: mkDoubleList(action.payload),
            };
        case Consts.SET_EDITING_CELLS:
            return {
                ...state,
                editingCells: action.payload,
            };
        default:
            return state
    }
}
