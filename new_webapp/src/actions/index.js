import * as Consts from "../constants/action-types";
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
export const addCandidate = makeActionCreator(Consts.ADD_CANDIDATE, "payload");
export const addFact = makeActionCreator(Consts.ADD_FACT, "payload");
export const decreaseArgs = makeActionCreator(Consts.DECREASE_ARGS);
export const increaseArgs = makeActionCreator(Consts.INCREASE_ARGS);
export const setArgNum = makeActionCreator(Consts.SET_ARG_NUM, "payload");
export const setSearchTypeInternal = makeActionCreator(Consts.SET_SEARCH_TYPE, "payload");
export const setEditingCells = makeActionCreator(Consts.SET_EDITING_CELLS, "payload");
export const setFacts = makeActionCreator(Consts.SET_FACTS, "payload");
export const updateCandidateUsageTable = makeActionCreator(Consts.UPDATE_CANDIDATE_USAGE, "payload");

export const setModalOpen = makeActionCreator(Consts.MODAL_OPEN);
export const setModalClosed = makeActionCreator(Consts.MODAL_CLOSE);

export const selectType = ({typeOption, examples}) => (dispatch) => {
    dispatch(setSearchTypeInternal({typeOption}));
    dispatch(setModalClosed());

    const serverPromise = hooglePlusTypeSearch({query:typeOption, examples});
    return handleCandidates(dispatch, serverPromise);
};

export const updateCandidateUsage = ({candidateId, usageId, args, code}) => (dispatch) => {
    dispatch(updateCandidateUsageTable({candidateId, usageId, args}));
    return ghciUsage({args, code})
        .then(backendResult =>
            dispatch(updateCandidateUsageTable({candidateId, usageId, ...backendResult})));
};

// This is where a request needs to be sent to the server
export const setSearchType = (payload) => (dispatch) => {
    dispatch(setSearchTypeInternal({...payload}));

    const serverPromise = hooglePlusTypeSearch({query: payload.query});
    return handleCandidates(dispatch, serverPromise);

};

const handleCandidates = (dispatch, serverPromise) => {
    return serverPromise.then(value => {
            const args = value.examples[0].usage.length - 1;
            dispatch(setArgNum(args));
            return value;
        })
        .then(value => dispatch(addCandidate(value)));
}