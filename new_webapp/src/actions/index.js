import {ADD_CANDIDATE, ADD_FACT, SEND_SEARCH, SET_FACTS, SET_EDITING_CELLS} from "../constants/action-types";

function makeActionCreator(type, ...argNames) {
    return function (...args) {
        const action = { type }
        argNames.forEach((arg, index) => {
            action[argNames[index]] = args[index]
        })
        return action
    }
}

// Creates an action creator like:
// export const setFacts = (payload) => {
//     return {
//         type: SET_FACTS,
//         payload,
//     }
// };

// Simple action creators for moving state around.
export const addCandidate = makeActionCreator(ADD_CANDIDATE, "payload");
export const addFact = makeActionCreator(ADD_FACT, "payload");
export const setFacts = makeActionCreator(SET_FACTS, "payload");
export const setEditingCells = makeActionCreator(SET_EDITING_CELLS, "payload");

// This is where a request needs to be sent to the server
export const sendSearch = (payload) => (dispatch) => {
    dispatch({
        type: SEND_SEARCH,
        payload,
    });
    const mockCandidate =     {
        code: "\\arg0 arg1-> catMaybes (listToMaybe arg0) arg1",
        examples: [
            ["z", "2", "zz"],
            ["asdf", "0", ""],
            ["qwop", "-1", "error"],
        ]
    };
    return (new Promise(resolve => setTimeout(resolve, 1000))
        .then(value => dispatch(addCandidate(mockCandidate))));
};