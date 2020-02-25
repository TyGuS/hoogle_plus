import {ADD_CANDIDATE, ADD_FACT, SET_SEARCH_TYPE, SET_FACTS, SET_EDITING_CELLS, SET_NEW_SEARCH, UPDATE_CANDIDATE_USAGE, INCREASE_ARGS, DECREASE_ARGS, SET_ARG_NUM} from "../constants/action-types";
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
export const decreaseArgs = makeActionCreator(DECREASE_ARGS);
export const increaseArgs = makeActionCreator(INCREASE_ARGS);
export const setArgNum = makeActionCreator(SET_ARG_NUM, "payload");
export const setSearchTypeInternal = makeActionCreator(SET_SEARCH_TYPE, "payload");
export const setEditingCells = makeActionCreator(SET_EDITING_CELLS, "payload");
export const setFacts = makeActionCreator(SET_FACTS, "payload");
export const updateCandidateUsageTable = makeActionCreator(UPDATE_CANDIDATE_USAGE, "payload");


export const updateCandidateUsage = ({candidateId, usageId, args, code}) => (dispatch) => {
    dispatch(updateCandidateUsageTable({candidateId, usageId, args}));
    return ghciUsage({args, code})
        .then(backendResult =>
            dispatch(updateCandidateUsageTable({candidateId, usageId, ...backendResult})));
};

// This is where a request needs to be sent to the server
export const setSearchType = (payload) => (dispatch) => {
    dispatch(setSearchTypeInternal({...payload}));

    return hooglePlusTypeSearch({query: payload.query})
        .then(value => {
            const args = value.examples[0].usage.length - 1;
            dispatch(setArgNum, args);
            return value;
        })
        .then(value => dispatch(addCandidate(value)));
};