import * as Consts from "../constants/action-types";
import _ from "underscore";
import { Search, ghciUsage, hooglePlusExampleSearch, hooglePlusMoreExamples } from "../gateways";
import { namedArgsToUsage, getArgCount, usageToExample } from "../utilities/args";
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
export const clearResults = makeActionCreator(Consts.CLEAR_RESULTS);
export const clearSpec = makeActionCreator(Consts.CLEAR_SPEC);
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

export const selectTypeFromOptions = ({typeOption}) => (dispatch, getState) => {
    const {spec} = getState();
    const examples = spec.rows.map(row => usageToExample(row.usage));
    dispatch(setSearchType({query: typeOption}));
    dispatch(setModalClosed());
    return dispatch(doSearch({query: typeOption, examples}));
}

// Update the search type and associated state.
export const setSearchType = ({query}) => (dispatch, getState) => {
    const {spec} = getState();
    const argCount = getArgCount(query);
    if (spec.numArgs !== argCount) {
        dispatch(setArgNum(argCount));
    }
    dispatch(setSearchTypeInternal({query}));
};

// This is where a request needs to be sent to the server
// query: str; examples: [{inputs:[str], output:str}]
export const doSearch = ({query, examples}) => (dispatch) => {
    dispatch(setSearchStatus({status:LOADING}))
    dispatch(clearResults());
    Search.getCodeCandidates({query, examples}, (candidate => {
        dispatch(addCandidate(candidate));
    }))
    .then(result => {
        return dispatch(setSearchStatus({status:DONE}));
    })
    .catch(error => {
        return dispatch(setSearchStatus({
            status: ERROR,
            errorMessage: error.message
        }));
    });
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