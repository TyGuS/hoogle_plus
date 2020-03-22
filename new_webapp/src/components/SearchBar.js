import _ from "underscore";
import React, {Component } from "react";
import {connect} from "react-redux";
import {setSearchType, getTypesFromExamples, doSearch, setExamples, doStop} from "../actions/index";
import ExampleTable from "./ExampleTable";
import { TypeSelection } from "./TypeSelection";
import { Button, InputGroup, FormControl, Form, Tooltip, Overlay, OverlayTrigger } from "react-bootstrap";
import { getDefaultFeatures } from "../utilities/featureManager";
import { usageToExample } from "../utilities/args";
import { LOADING, ERROR } from "../constants/fetch-states";

const mapDispatchToProps = (dispatch) => {
    return {
        setSearchType: ({query}) => dispatch(setSearchType({query})),
        doSearch: ({query, examples}) => dispatch(doSearch({query, examples})),
        getTypesFromExamples: usages => dispatch(getTypesFromExamples(usages)),
        clearExamples: _ => dispatch(setExamples([])),
        doStop: ({id}) => dispatch(doStop({id})),
    }
}

const mapStateToProps = (state) => {
    return {
        searchType: state.spec.searchType,
        exampleRows: state.spec.rows,
        errorMessage: state.spec.errorMessage,
        searchStatus: state.spec.searchStatus,
        isEditing: !!state.spec.editingExampleRow,
        queryId: state.candidates.queryId,
    }
};

const connectedSearchBar = (props) => {
    const {searchType, exampleRows, searchStatus, errorMessage, isEditing, queryId} = props;
    const {setSearchType, doSearch, getTypesFromExamples, clearExamples, doStop} = props;
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
            getTypesFromExamples(filteredUsages);
            return;
        }
        doSearch({query: searchType, examples: filteredUsages});
    };

    const handleStop = (event) => {
        doStop({id: queryId})
    };

    const hasAnExample = !_.isEmpty(exampleRows);

    const canSubmit = () => {
        const anyIncompleteExamples = _.any(exampleRows, ({inputs, output}) => {
            return _.any([output, ...inputs], x => (_.isNull(x) || _.isUndefined(x)));
        });
        const hasAType = !isMissingType(searchType);
        return (hasAnExample || hasAType) && !anyIncompleteExamples && !isEditing;
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
        switch(searchStatus) {
            case ERROR:
                return errorMessage || "Error";
            case LOADING:
                return "Getting results...";
            default:
                return "Search";
        }
    };

    return (
        <div>
            <TypeSelection hidden={!search.permitTypeCandidates} />
            <div className="container">
                <Form onSubmit={handleSubmit}>
                <div className="row justify-content-center">
                <InputGroup className="mb-3 col-8">
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
                <div
                    className="row justify-content-center"
                    hidden={!search.permitExamples}>
                    <div className="col">
                        <div>
                            Example Specifications:
                        </div>
                        <ExampleTable/>
                    </div>
                </div>

                <div className="row justify-content-center">
                    <div className="col-10">
                        {hasAnExample ? (
                            <Button
                                className="float-right"
                                variant="link"
                                onClick={clearExamples}
                            >
                                Clear Examples
                            </Button>):<></>}
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
                            disabled={queryId==null}
                            onClick={handleStop}
                            type="button">
                            Stop
                        </Button>
                    </div>
                </div>
                </Form>
            </div>
        </div>
    );
}

const SearchBar = connect(mapStateToProps, mapDispatchToProps)(connectedSearchBar);

export default SearchBar;
