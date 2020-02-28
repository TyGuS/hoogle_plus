import * as Consts from "../constants/action-types";
import * as _ from "underscore";
import { getArgNames } from "../utilities/args";

export const initialSpecState = {
    editingCells: [],
    rows: [{
        id: "0",
        usage: ["x", "2", "xx"]
    }],
    numArgs: 2,
    searchType: null,
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

const dedupeSpecs = (rows, new_rows) => {
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
        case Consts.SET_FACTS:
            return {
                ...state,
                rows: mkDoubleList(action.payload, state.numArgs),
            };
        case Consts.SET_EDITING_CELLS:
            return {
                ...state,
                editingCells: action.payload,
            };
        case Consts.SET_SEARCH_TYPE:
            return {
                ...state,
                searchType: action.payload.query,
            }
        default:
            return state
    }
}
