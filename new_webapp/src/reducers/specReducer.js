import * as Consts from "../constants/action-types";
import _ from "underscore";
import { namedArgsToExample } from "../utilities/args";
import { DONE, LOADING, ERROR } from "../constants/fetch-states";

export const initialSpecState = {
    editingExampleRow: null, // id-str
    rows: [],
    // rows: [{inputs:[str], output:str, id}]
    numArgs: 2,
    errorMessage: null,
    searchStatus: DONE,
    searchType: "",
    searchTypeOptions: [],
    searchPromise: null, // {abort: () -> (), ready: Promise}
};

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
    });
    return r;
};

const clearErrors = (state) => {
    return {
        ...state,
        errorMessage: null,
        searchStatus: DONE,
    }
};

// When numArgs changes, we need to insert undefined or removed arguments.
const updateRows = (oldRows, numArgs) => {
    return oldRows.map(row => {
        let newInputs = [];
        for(let i = 0; i < numArgs; i++) {
            if (i < row.inputs.length) {
                newInputs = newInputs.concat(row.inputs[i]);
            } else {
                newInputs = newInputs.concat(undefined);
            }
        }
        return {
            ...row,
            inputs: newInputs
        };
    });
};

export function specReducer(state = initialSpecState, action) {
    switch(action.type) {
        case Consts.SET_SEARCH_STATUS:
            switch(action.payload.status) {
                case LOADING:
                case DONE:
                    return {
                        ...state,
                        searchStatus: action.payload.status,
                    };
                case ERROR:
                    return {
                        ...state,
                        searchStatus: ERROR,
                        errorMessage: action.payload.errorMessage,
                    };
                default:
                    console.error("invalid payload", action.payload);
                    return state;
            }
        case Consts.INCREASE_ARGS:
            var newNumArgs = state.numArgs + 1;
            return {
                ...clearErrors(state),
                numArgs: newNumArgs,
                rows: updateRows(state.rows, newNumArgs),
            };
        case Consts.DECREASE_ARGS:
            var newNumArgs = Math.max(state.numArgs - 1, 1);
            return {
                ...clearErrors(state),
                numArgs: newNumArgs,
                rows: updateRows(state.rows, newNumArgs),
            };
        case Consts.SET_ARG_NUM:
            return {
                ...clearErrors(state),
                numArgs: action.payload,
                rows: updateRows(state.rows, action.payload),
            };
        case Consts.ADD_EXAMPLE:
            return {
                ...state,
                rows: dedupeSpecs(state.rows, [action.payload]),
            };
        case Consts.SET_EXAMPLES:
            return {
                ...state,
                rows: namedArgsToExample(action.payload, state.numArgs),
            };
        case Consts.SET_EXAMPLE_EDITING_ROW:
            return {
                ...state,
                editingExampleRow: action.payload,
            };
        case Consts.SET_SEARCH_TYPE:
            return {
                ...clearErrors(state),
                searchType: action.payload.query,
            }
        case Consts.SET_TYPE_OPTIONS:
            return {
                ...clearErrors(state),
                searchTypeOptions: action.payload,
            }
        case Consts.SET_SEARCH_PROMISE:
            return {
                ...state,
                searchPromise: action.payload,
            }
        default:
            return state
    }
}
