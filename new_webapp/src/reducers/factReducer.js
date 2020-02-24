import * as Consts from "../constants/action-types";
import * as _ from "underscore";

export const initialFactState = {
    editingCells: [],
    rows: [{
        id: "1",
        usage: ["x", "2", "xx"]
    }]
};

const mkDoubleList = (listList) => {
    return listList.map(element => {
        return {
            usage: [
                element.arg0,
                element.arg1,
                element.arg2,
            ],
            id: element.id,
        }
    });
}

const dedupeFacts = (rows, new_rows) => {
    let ids = new Set();
    rows.forEach((row) => ids.add(row.id))
    let r = rows;
    new_rows.forEach(nr => {
        if (! ids.has(nr.id)) {
            r = r.concat(nr);
        }
    });
    return r;
}

export function factReducer(state = initialFactState, action) {
    switch(action.type) {
        case Consts.ADD_FACT:
            return {
                ...state,
                rows: dedupeFacts(state.rows, [action.payload]),
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
