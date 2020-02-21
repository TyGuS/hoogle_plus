import {ADD_CANDIDATE, ADD_FACT, SEND_SEARCH} from "../constants/action-types";

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

export const sendSearch = (payload) => (dispatch) => {
    dispatch({
        type: SEND_SEARCH,
        payload,
    });
    return Promise.resolve({f:"x"}).then(value => dispatch(addFact(value)))
};