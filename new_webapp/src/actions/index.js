import {ADD_CANDIDATE, ADD_FACT, SEND_SEARCH, SET_FACTS, SET_EDITING_CELLS, SET_NEW_SEARCH, UPDATE_CANDIDATE_USAGE} from "../constants/action-types";
import { hooglePlusTypeSearch, ghciUsage } from "../gateways";

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

export const newSearch = makeActionCreator(SET_NEW_SEARCH, "payload");
export const updateCandidateUsageTable = makeActionCreator(UPDATE_CANDIDATE_USAGE, "payload");


export const updateCandidateUsage = ({candidateId, usageId, args, code}) => (dispatch) => {
    dispatch(updateCandidateUsageTable({candidateId, usageId, args}));
    return ghciUsage({args, code})
        .then(backendResult =>
            dispatch(updateCandidateUsageTable({candidateId, usageId, ...backendResult})));
};

// This is where a request needs to be sent to the server
export const sendSearch = (payload) => (dispatch) => {
    dispatch(newSearch({...payload}));

    return hooglePlusTypeSearch({query: payload.query})
        .then(value => dispatch(addCandidate(value)));
};