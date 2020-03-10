import * as Consts from "../constants/action-types";
import _ from "underscore";
import { Search, ghciUsage, hooglePlusExampleSearch, hooglePlusMoreExamples } from "../gateways";
import { namedArgsToUsage, getArgCount } from "../utilities/args";
import { LOADING, DONE, ERROR } from "../constants/fetch-states";

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

export const setExampleEditingRow = makeActionCreator(Consts.SET_EXAMPLE_EDITING_ROW, "payload");
export const setExamples = makeActionCreator(Consts.SET_EXAMPLES, "payload");
export const updateCandidateUsageTable = makeActionCreator(Consts.UPDATE_CANDIDATE_USAGE, "payload");
export const fetchMoreCandidateUsages = makeActionCreator(Consts.FETCH_MORE_CANDIDATE_USAGES, "payload");

export const setSearchTypeInternal = makeActionCreator(Consts.SET_SEARCH_TYPE, "payload");
export const setSearchStatus = makeActionCreator(Consts.SET_SEARCH_STATUS, "payload");

export const setModalOpen = makeActionCreator(Consts.MODAL_OPEN);
export const setModalClosed = makeActionCreator(Consts.MODAL_CLOSE);
export const setTypeOptions = makeActionCreator(Consts.SET_TYPE_OPTIONS, "payload");

export const decreaseArgs = makeActionCreator(Consts.DECREASE_ARGS);
export const increaseArgs = makeActionCreator(Consts.INCREASE_ARGS);
export const setArgNum = makeActionCreator(Consts.SET_ARG_NUM, "payload");

// A user selected a type option. Close the modal and start the search!
export const selectType = ({typeOption, examples}) => (dispatch) => {
    dispatch(setSearchTypeInternal({query: typeOption}));
    dispatch(setModalClosed());

    Search.getCodeCandidates({query: typeOption, examples}, (candidate => {
        dispatch(addCandidate(candidate));
    })).then(_ => dispatch(setSearchStatus(DONE)));
    return;
};

// When a new candidate's usage changes, go and get new outputs for each
// usage across all current candidates.
export const updateCandidateUsages = ({usageId, args}) => (dispatch, getState) => {
    const {candidates, spec} = getState();
    const typeSignature = spec.searchType;
    if(! _.isEmpty(candidates.results)) {
        candidates.results.map(candidate => {
            return dispatch(updateCandidateUsage({
                candidateId: candidate.candidateId,
                code: candidate.code,
                usageId,
                args,
                typeSignature,
            }));
        })
    } else {
        debugger;
    }
}

// Go and get a new output for a particular new input.
export const updateCandidateUsage = ({typeSignature, candidateId, usageId, args, code}) => (dispatch, getState) => {
    dispatch(updateCandidateUsageTable({candidateId, usageId, args}));

    return ghciUsage({typeSignature, args, code})
        .then(backendResult =>
            dispatch(updateCandidateUsageTable({candidateId, usageId, ...backendResult})));
};

export const setSearchType = ({query}) => (dispatch, getState) => {
    const numArgs = getArgCount(query);
    const {spec} = getState();
    if (spec.numArgs !== numArgs) {
        dispatch(setArgNum(numArgs));
    }
    dispatch(setSearchTypeInternal({query}));
    return;
}

// This is where a request needs to be sent to the server
// query: str; examples: [{inputs:[str], output:str}]
export const runSearch = ({query, examples}) => (dispatch) => {
    setSearchType({query});

    Search.getCodeCandidates({query, examples}, (candidate => {
        dispatch(addCandidate(candidate));
    }))
    .then(_ => dispatch(setSearchStatus(DONE)))
    .catch(_ => dispatch(setSearchStatus(ERROR)));
    return;
};

// A user gave us some examples. We need to get some possible query options
// and present them.
// [{inputs:[str], output:str}]
export const getTypesFromExamples = (examples) => (dispatch) => {
    return Search.getTypeCandidates({examples})
        .then(value => {
            if (value["typeCandidates"]) {
                dispatch(setTypeOptions(value.typeCandidates));
                dispatch(setModalOpen());
            } else {
                debugger;
            }
        });
}

// Get more example usages for this particular candidate.
// usages: [{inputs:[str], output:str}]
export const getMoreExamples = ({candidateId, code, usages}) => (dispatch, getState) => {
    const {spec} = getState();
    dispatch(fetchMoreCandidateUsages({candidateId, status: LOADING}));
    return hooglePlusMoreExamples({code, usages, queryType: spec.searchType})
        .then(results => {
            const {examples} = results;
            return dispatch(fetchMoreCandidateUsages({
                candidateId,
                status: DONE,
                result: examples
            }));
        })
        .catch(errorResult => {
            console.error("getMoreExamples failed", errorResult);
            return dispatch(fetchMoreCandidateUsages({
                candidateId,
                status: ERROR,
                message: errorResult.error || "Unknown Error",
            }));
        })
}