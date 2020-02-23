import {ADD_CANDIDATE, ADD_FACT, SEND_SEARCH, SET_FACTS, SET_EDITING_CELLS} from "../constants/action-types";

export const addCandidate = (payload) => {
    return {
        type: ADD_CANDIDATE,
        payload,
    }
};

export const addFact = (payload) => {
    return {
        type: ADD_FACT,
        payload,
    }
};

export const setFacts = (payload) => {
    return {
        type: SET_FACTS,
        payload,
    }
};

export const setEditingCells = (payload) => {
    return {
        type: SET_EDITING_CELLS,
        payload,
    }
};

export const sendSearch = (payload) => (dispatch) => {
    dispatch({
        type: SEND_SEARCH,
        payload,
    });
    return Promise.resolve({f:"x"}).then(value => dispatch(addFact(value)))
};