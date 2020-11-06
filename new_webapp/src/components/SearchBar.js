import _ from "underscore";
import React, {Component } from "react";
import {connect} from "react-redux";
import OutsideClickHandler from "react-outside-click-handler";
import {setSearchType, getTypesFromExamples, doSearch, setExamples, doStop, 
    setExampleEditingRow, setExampleEditingCol, refineSearch, markClean} from "../actions/index";
import ExampleTable from "./ExampleTable";
import { TypeSelection } from "./TypeSelection";
import { Button, InputGroup, FormControl, Form, Tooltip, Overlay, OverlayTrigger, Alert } from "react-bootstrap";
import { getDefaultFeatures } from "../utilities/featureManager";
import { LOADING, ERROR } from "../constants/fetch-states";

const mapDispatchToProps = (dispatch) => {
    return {
        setSearchType: ({query}) => dispatch(setSearchType({query})),
        doSearch: ({query, examples}) => dispatch(doSearch({query, examples})),
        getTypesFromExamples: (usages, argNames) => dispatch(getTypesFromExamples(usages, argNames)),
        unfocusEditingRow: _ => {
            dispatch(setExampleEditingRow(null));
            dispatch(setExampleEditingCol(null));
        },
        doStop: ({id}) => dispatch(doStop({id})),
        refineSearch: ({query, examples}) => dispatch(refineSearch({query, examples})),
        markClean: _ => dispatch(markClean()),
    }
}

const mapStateToProps = (state) => {
    return {
        isDirty: state.candidates.isDirty,
        searchType: state.spec.searchType,
        exampleRows: state.spec.rows,
        errorMessage: state.spec.errorMessage,
        searchStatus: state.spec.searchStatus,
        isEditing: !!state.spec.editingExampleRow,
        argNames: state.spec.argNames,
        queryId: state.candidates.queryId,
        canStop: !!state.spec.searchPromise,
        hasResults: state.candidates.results.length > 0,
    }
};

const connectedSearchBar = (props) => {
    const { searchType, exampleRows, searchStatus, errorMessage, isEditing, queryId, argNames } = props;
    const { setSearchType, doSearch, getTypesFromExamples, doStop, unfocusEditingRow } = props;
    const {search} = getDefaultFeatures();

    const filteredUsages = _.filter(exampleRows, row => !_.any(row.inputs, _.isUndefined) && !_.isUndefined(row.output));

    const isMissingType = (queryStr) => !queryStr || queryStr.trim() === "";

    const handleChange = (event) => {
        const value = event.target.value;
        setSearchType({query: value});
    }

    const handleSubmit = (event) => {
        event.preventDefault();
        if (isMissingType(searchType)) {
            getTypesFromExamples(filteredUsages, argNames);
            return;
        }
        props.markClean();
        if (props.isDirty && props.hasResults) {
            return props.refineSearch({query: searchType, examples: filteredUsages});
        }
        return doSearch({query: searchType, examples: filteredUsages});
    };

    const handleStop = (event) => {
        props.markClean();
        doStop({id: queryId})
    };

    const hasAnExample = !_.isEmpty(exampleRows);

    const canSubmit = () => {
        const anyIncompleteExamples = _.any(exampleRows, ({inputs, output}) => {
            return _.any([output, ...inputs], x => (_.isNull(x) || _.isUndefined(x)));
        });
        const hasAType = !isMissingType(searchType);
        return (hasAnExample || hasAType) && !anyIncompleteExamples;
    };

    const buttonVariant = () => {
        switch(searchStatus) {
            case ERROR:
                return "danger";
            default:
                return "primary";
        }
    };

    const buttonContent = () => {
        if (props.isDirty && props.hasResults) {
            return "Refine results";
        }
        switch(searchStatus) {
            case LOADING:
                return "Getting results...";
            default:
                return "Search";
        }
    };

    const alert = () => {
        if (searchStatus === ERROR) {
            const errors = errorMessage.split('\n');
            const beautifulError = (
                <div className="text-left">
                    {errors.map(e => (<p>{e}</p>))}
                </div>
            );
            return (
                <Alert variant="danger" className="mt-3">
                    {beautifulError || "Unknown Error"}
                </Alert>
            )
        }
        return (<></>);
    }

    return (
        <div className="container px-4 py-3">
            <TypeSelection hidden={!search.permitTypeCandidates} />
            <div>
                <Form onSubmit={handleSubmit}>
                <div className="mb-3">
                    <div className="text-left mb-3 font-weight-bold">
                        Type Query
                    </div>
                    <div>
                        <InputGroup className="mb-0">
                            <FormControl
                            aria-label="Default"
                            aria-describedby="inputGroup-sizing-default"
                            placeholder="Search by type here"
                            className="text-center"
                            value={searchType}
                            onChange={handleChange}
                            />
                        </InputGroup>
                    </div>
                </div>
                <div
                    className="justify-content-center"
                    hidden={!search.permitExamples}>
                    <div className="text-left mb-3 font-weight-bold">
                        Tests
                    </div>
                    <div className="px-0">
                        <OutsideClickHandler
                            onOutsideClick={() => isEditing ? unfocusEditingRow() : undefined}>
                            <ExampleTable/>
                        </OutsideClickHandler>
                    </div>
                </div>

                <div className="row justify-content-center mt-5">
                    <div className="col-10">
                        <Button
                            className="mr-2"
                            disabled={!canSubmit()}
                            onClick={handleSubmit}
                            variant={buttonVariant()}
                            type="submit"
                        >
                            {buttonContent()}
                        </Button>
                        <Button
                            disabled={!props.canStop}
                            onClick={handleStop}
                            type="button">
                            Stop
                        </Button>
                        {alert()}
                    </div>
                </div>
                </Form>
            </div>
        </div>
    );
}

const SearchBar = connect(mapStateToProps, mapDispatchToProps)(connectedSearchBar);

export default SearchBar;
