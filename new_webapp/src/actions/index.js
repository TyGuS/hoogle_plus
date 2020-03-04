import * as Consts from "../constants/action-types";
import _ from "underscore";
import { Search, ghciUsage, hooglePlusExampleSearch, hooglePlusMoreExamples } from "../gateways";
import { namedArgsToUsage, getArgCount } from "../utilities/args";
import { LOADING, DONE } from "../constants/fetch-states";

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
export const setExampleEditingRow = makeActionCreator(Consts.SET_EXAMPLE_EDITING_ROW, "payload");
export const setExamples = makeActionCreator(Consts.SET_EXAMPLES, "payload");
export const updateCandidateUsageTable = makeActionCreator(Consts.UPDATE_CANDIDATE_USAGE, "payload");
export const fetchMoreCandidateUsages = makeActionCreator(Consts.FETCH_MORE_CANDIDATE_USAGES, "payload");

export const setSearchStatus = makeActionCreator(Consts.SET_SEARCH_STATUS, "payload");

export const setModalOpen = makeActionCreator(Consts.MODAL_OPEN);
export const setModalClosed = makeActionCreator(Consts.MODAL_CLOSE);
export const setTypeOptions = makeActionCreator(Consts.SET_TYPE_OPTIONS, "payload");

export const selectType = ({typeOption, examples}) => (dispatch) => {
    dispatch(setSearchTypeInternal({searchType: typeOption}));
    dispatch(setModalClosed());

    Search.getCodeCandidates({query: typeOption, examples}, (candidate => {
        dispatch(addCandidate(candidate));
    })).then(_ => dispatch(setSearchStatus(DONE)));
    return;
};

export const updateCandidateUsages = ({usageId, args}) => (dispatch, getState) => {
    const {candidates} = getState();
    if(! _.isEmpty(candidates.results)) {
        candidates.results.map(candidate => {
            return dispatch(updateCandidateUsage({
                candidateId: candidate.candidateId,
                usageId: usageId,
                code: candidate.code,
                args: args
            }));
        })
    } else {
        debugger;
    }
}

export const updateCandidateUsage = ({candidateId, usageId, args, code}) => (dispatch, getState) => {
    dispatch(updateCandidateUsageTable({candidateId, usageId, args}));

    return ghciUsage({args, code})
        .then(backendResult =>
            dispatch(updateCandidateUsageTable({candidateId, usageId, ...backendResult})));
};

// This is where a request needs to be sent to the server
export const setSearchType = (payload) => (dispatch) => {
    const argCount = getArgCount(payload.query);
    dispatch(setArgNum(argCount));
    dispatch(setSearchTypeInternal({...payload}));

    Search.getCodeCandidates({query: payload.query}, (candidate => {
        dispatch(addCandidate(candidate));
    })).then(_ => dispatch(setSearchStatus(DONE)));
    return;
};

export const getTypesFromExamples = (payload) => (dispatch) => {
    const {examples} = payload;
    return Search.getTypeCandidates({examples})
        .then(value => {
            if (value["typeCandidates"]) {
                dispatch(setModalOpen());
                dispatch(setTypeOptions(value.typeCandidates));
            } else {
                debugger;
            }
        });
}

export const getMoreExamples = ({candidateId, code, usages}) => (dispatch, getState) => {
    const {spec} = getState();
    dispatch(fetchMoreCandidateUsages({candidateId, status: LOADING}));
    return hooglePlusMoreExamples({code, usages, queryType: spec.searchType})
        .then(({examples}) => {
            return dispatch(fetchMoreCandidateUsages({
                candidateId,
                status: DONE,
                result: examples
            }));
        });
}