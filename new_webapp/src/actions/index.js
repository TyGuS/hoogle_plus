import * as Consts from "../constants/action-types";
import _ from "underscore";
import { Search, ghciUsage, hooglePlusExampleSearch, hooglePlusMoreExamples } from "../gateways";
import { namedArgsToUsage, getArgCount, usageToExample } from "../utilities/args";
import { LOADING, DONE, ERROR } from "../constants/fetch-states";
import { log } from "../utilities/logger";
import { v4 } from "uuid";
import { defaultExamplesShownIncrement } from "../utilities/featureManager";
import search from "../gateways/search";

function makeActionCreator(type, ...argNames) {
    return function (...args) {
        const action = { type }
        argNames.forEach((arg, index) => {
            action[argNames[index]] = args[index]
        })
		log.info(type,action)
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
export const filterResults = makeActionCreator(Consts.FILTER_RESULTS, "payload");
export const addFact = makeActionCreator(Consts.ADD_EXAMPLE, "payload");

export const setExampleEditingRow = makeActionCreator(Consts.SET_EXAMPLE_EDITING_ROW, "payload");
export const setExamples = makeActionCreator(Consts.SET_EXAMPLES, "payload");
export const updateCandidateUsageTable = makeActionCreator(Consts.UPDATE_CANDIDATE_USAGE, "payload");
const addCandidateUsageInternal = makeActionCreator(Consts.ADD_CANDIDATE_USAGE, "payload");
export const fetchMoreCandidateUsages = makeActionCreator(Consts.FETCH_MORE_CANDIDATE_USAGES, "payload");
export const showMoreCandidateUsages = makeActionCreator(Consts.SHOW_MORE_USAGES, "payload");

const setSearchTypeInternal = makeActionCreator(Consts.SET_SEARCH_TYPE, "payload");
export const setSearchStatus = makeActionCreator(Consts.SET_SEARCH_STATUS, "payload");
export const stopSearch = makeActionCreator(Consts.STOP_SEARCH, "payload");
export const setSearchPromise = makeActionCreator(Consts.SET_SEARCH_PROMISE, "payload");

export const setModalOpen = makeActionCreator(Consts.MODAL_OPEN);
export const setModalClosed = makeActionCreator(Consts.MODAL_CLOSE);
export const setTypeOptions = makeActionCreator(Consts.SET_TYPE_OPTIONS, "payload");

export const decreaseArgs = makeActionCreator(Consts.DECREASE_ARGS);
export const increaseArgs = makeActionCreator(Consts.INCREASE_ARGS);
export const setArgNum = makeActionCreator(Consts.SET_ARG_NUM, "payload");

// When a new candidate's usage changes, go and get new outputs for each
// usage across all current candidates.
export const updateCandidateUsages = ({usageId, inputs}) => (dispatch, getState) => {
    const {candidates, spec} = getState();
    const typeSignature = spec.searchType;
    if(! _.isEmpty(candidates.results)) {
        candidates.results.map(candidate => {
            return dispatch(updateCandidateUsage({
                candidateId: candidate.candidateId,
                code: candidate.code,
                usageId,
                inputs,
                typeSignature,
            }));
        });
    } else {
        console.error("updating usage without results! How was this fired?");
    }
};

export const addCandidateUsage = ({inputs}) => (dispatch, getState) => {
    const {candidates} = getState();
    const usageId = v4();
    candidates.results.forEach(({candidateId}) => {
        dispatch(addCandidateUsageInternal({
            candidateId,
            usageId,
            inputs,
        }));
    });
    // Must sequence the fetch for after
    // we've added the new usage to each candidate.
    dispatch(updateCandidateUsages({usageId, inputs}));
};

// Go and get a new output for a particular new input.
export const updateCandidateUsage = ({typeSignature, candidateId, usageId, inputs, code}) => (dispatch, getState) => {
    dispatch(updateCandidateUsageTable({candidateId, usageId, inputs}));

    return ghciUsage({typeSignature, inputs, code})
        .then(({result}) => {
            return dispatch(updateCandidateUsageTable({candidateId, usageId, output: result}))
        })
        .catch(({error}) => {
            return dispatch(updateCandidateUsageTable({candidateId, usageId, error}))
        });
};

export const selectTypeFromOptions = ({typeOption}) => (dispatch, getState) => {
    const {spec} = getState();
    const examples = spec.rows;
    dispatch(setSearchType({query: typeOption}));
    dispatch(setModalClosed());
    return dispatch(doSearch({query: typeOption, examples}));
}

// Update the search type and associated state.
export const setSearchType = ({query}) => (dispatch, getState) => {
    const {spec, candidates} = getState();
    dispatch(doStop({id: candidates.id}));
    const argCount = getArgCount(query);
    if (spec.numArgs !== argCount) {
        dispatch(setArgNum(argCount));
    }
    dispatch(setSearchTypeInternal({query}));
};

// This is where a request needs to be sent to the server
// query: str; examples: [{inputs:[str], output:str}]
export const doSearch = ({query, examples}) => (dispatch) => {
    dispatch(setSearchStatus({status:LOADING, searchType: query, examples}));
    dispatch(filterResults({examples}));
    const {abort, ready} = Search.getCodeCandidates({query, examples}, (candidate => {
            if (!candidate.error) {
                dispatch(addCandidate(candidate));
            }
        }));
    const readyPromise = ready
        .then(result => {
            try {
                const firstResult = JSON.parse(result.trim().split("\n")[0]);
                if (firstResult.error) {
                    return Promise.reject({message: firstResult.error});
                }
            } catch (error) {
                console.error("doSearch result error", error);
            }
            return dispatch(setSearchStatus({status:DONE}));
        })
        .catch(error => {
            if (error.name && error.name === "AbortError") {
                return;
            }
            return dispatch(setSearchStatus({
                status: ERROR,
                errorMessage: error.message
            }));
        })
        .finally(_ => dispatch(setSearchPromise(null)));
    const searchPromise = {
        abort,
        ready: readyPromise,
    };
    dispatch(setSearchPromise(searchPromise));
    return;
};

// A user gave us some examples. We need to get some possible query options
// and present them.
// [{inputs:[str], output:str}]
export const getTypesFromExamples = (examples) => (dispatch) => {
    dispatch(setSearchStatus({status:LOADING}));
    dispatch(filterResults({examples}));
    const {abort, ready} = Search.getTypeCandidates({examples});
    const readyPromise = ready
        .then(value => {
            if (value["typeCandidates"]) {
                dispatch(setTypeOptions(value.typeCandidates));
                dispatch(setModalOpen());
                dispatch(setSearchStatus({status:DONE}));
            } else {
                console.error("getTypesFromExamples: no candidates, but successful response.");
            }
        })
        .catch(error => {
            if (error.name && error.name === "AbortError") {
                return;
            }
            console.error("getTypesFromExamples failed", error);
            dispatch(setSearchStatus({status:ERROR, errorMessage: error.toString()}));
        })
        // .finally(_ => dispatch(setSearchPromise(null)));
    dispatch(setSearchPromise({abort, ready:readyPromise}));
}

// Get more example usages for this particular candidate.
// usages: [{inputs:[str], output:str}]
export const getMoreExamples = ({candidateId, code, examples}) => (dispatch, getState) => {
    const {spec, candidates} = getState();
    const candidate = _.findWhere(candidates.results, {candidateId});

    const currentExamplesCount = candidate.examples.length;
    const currentExamplesShown = candidate.examplesShown;
    const wantToShow = currentExamplesShown + defaultExamplesShownIncrement;
    // If there are more examples stored, then just show them.
    if (wantToShow <= currentExamplesCount) {
        return dispatch(showMoreCandidateUsages({candidateId, newValue: wantToShow}));
    }

    // Else fetch more
    dispatch(fetchMoreCandidateUsages({candidateId, status: LOADING}));
    return hooglePlusMoreExamples({code, examples, queryType: spec.searchType})
        .then(results => {
            dispatch(showMoreCandidateUsages({candidateId, newValue: wantToShow}));
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

// id: uuid
export const doStop = ({id}) => (dispatch, getState) => {
    const {spec} = getState();
    if (spec.searchPromise) {
        spec.searchPromise.abort();
        dispatch(setSearchPromise(null));
    }
    dispatch(setSearchStatus({status:DONE}));
    if (id) {
        return Search.sendStopSignal({id})
            .catch(error => {
                console.error("doStop failed to sendStopSignal: ", error);
            })
            .finally(_ => {
                return dispatch(stopSearch({id}));
            })
    }
};
