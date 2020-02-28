import * as Consts from "../constants/action-types";
import * as _ from "underscore";
import { getArgNames } from "../utilities/args";

export const initialSpecState = {
    editingExampleRow: null,
    rows: [{
        id: "0",
        usage: ["x", "2", "xx"]
    }],
    numArgs: 2,
    searchTypeOptions: [
        "foo",
        "bar"
    ],
};

const mkDoubleList = (listList, numArgs) => {
    return listList.map(element => {
        const argNames = getArgNames(numArgs);
        const usage = argNames.map(argName => element[argName]).concat(element.result);
        return {
            usage: usage,
            id: element.id,
        }
    });
}

// Overwrite existing rows with their new_row, and add the rest of the new_rows.
// Don't change the order of rows on update!
const dedupeSpecs = (rows, new_rows) => {
    let seen_new_rows = new Set();
    let r = rows;
    r = r.map((row) => {
        for(var i = 0; i < new_rows.length; i++) {
            let new_row = new_rows[i];
            if (row.id === new_row.id) {
                seen_new_rows.add(new_row.id);
                return new_row;
            }
        }
        return row;
        });
    new_rows.forEach(row => {
        if(!seen_new_rows.has(row.id)) {
            r = r.concat(row);
        }
    })
    return r;
};


export function specReducer(state = initialSpecState, action) {
    switch(action.type) {
        case Consts.INCREASE_ARGS:
            return {
                ...state,
                numArgs: state.numArgs + 1
            };
        case Consts.DECREASE_ARGS:
            return {
                ...state,
                numArgs: state.numArgs > 1 ? state.numArgs - 1 : 1
            };
        case Consts.SET_ARG_NUM:
            return {
                ...state,
                numArgs: action.payload
            };
        case Consts.ADD_FACT:
            return {
                ...state,
                rows: dedupeSpecs(state.rows, [action.payload]),
            };
        case Consts.SET_EXAMPLES:
            return {
                ...state,
                rows: mkDoubleList(action.payload, state.numArgs),
            };
        case Consts.SET_EXAMPLE_EDITING_ROW:
            return {
                ...state,
                editingExampleRow: action.payload,
            };
        case Consts.SET_SEARCH_TYPE:
            return {
                ...state,
                searchType: action.payload.query,
            }
        case Consts.SET_TYPE_OPTIONS:
            return {
                ...state,
                searchTypeOptions: action.payload,
            }
        default:
            return state
    }
}
