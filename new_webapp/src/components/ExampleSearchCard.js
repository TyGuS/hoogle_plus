import React from "react";
import _ from "underscore";
import { connect } from "react-redux";
import { Card } from "react-bootstrap";
import { LOADING } from "../constants/fetch-states";
import ExampleSearchItem from "./ExampleSearchItem";
import { setSearchType, setExamples, setArgNames, doSearch, getTypesFromExamples } from "../actions/index";

const mapStateToProps = state => {
    return {
        searchStatus: state.spec.searchStatus,
        argNames: state.spec.argNames,
    };
}

const mapDispatchToProps = dispatch => {
    return {
        setSearchType: ({query}) => dispatch(setSearchType({query})), 
        setExamples: (examples) => dispatch(setExamples(examples)), 
        setArgNames: (argNames) => dispatch(setArgNames(argNames)), 
        doSearch: ({query, examples}) => dispatch(doSearch({query, examples})),
        getTypesFromExamples: (examples, argNames) => dispatch(getTypesFromExamples(examples, argNames)),
    }
};

const ConnectedExampleSearchCard = (props) => {
    const {argNames, searchStatus} = props;
    const {setSearchType, setExamples, setArgNames, doSearch, getTypesFromExamples} = props;

    const searchByTypeOnly = (
        <ExampleSearchItem
            description = "Search by type only"
            queryStr = "d: a -> xs: [Maybe a] -> a"
            examples = {[]}
            argNames = {argNames}
            setSearchType = {setSearchType}
            setExamples = {setExamples}
            setArgNames = {setArgNames}
            doSearch = {doSearch}
            getTypesFromExamples = {getTypesFromExamples}
        />
    );

    const searchByExampleOnly = (
        <ExampleSearchItem
            description = "Search by examples only"
            queryStr = ""
            examples = {
                [{
                    id: "ex-1",
                    inputs: ["1", "[Nothing, Nothing]"],
                    output: "1"
                },
                {
                    id: "ex-2",
                    inputs: ["1", "[Nothing, Just (-1), Nothing]"],
                    output: "-1"
                }]
            }
            argNames = {["d", "xs"]}
            setSearchType = {setSearchType}
            setExamples = {setExamples}
            setArgNames = {setArgNames}
            doSearch = {doSearch}
            getTypesFromExamples = {getTypesFromExamples}
        />
    );

    const searchByTypeAndExamples = (
        <ExampleSearchItem
            description = "Search by both type and examples"
            queryStr = "d: a -> xs: [Maybe a] -> a"
            examples = {
                [{
                    id: "ex-1",
                    inputs: ["1", "[Nothing, Nothing]"],
                    output: "1"
                },
                {
                    id: "ex-2",
                    inputs: ["1", "[Nothing, Just (-1), Nothing]"],
                    output: "-1"
                }]
            }
            argNames = {argNames}
            setSearchType = {setSearchType}
            setExamples = {setExamples}
            setArgNames = {setArgNames}
            doSearch = {doSearch}
            getTypesFromExamples = {getTypesFromExamples}
        />
    );

    // text in the card
    const title = "Example Searches";

    if(searchStatus !== LOADING) {
        return (
            <Card className="mb-5 text-left">
                <div className="container py-3 px-4">
                    <div>
                        <h5 className="mb-3">{title}</h5>
                        <p>
                            <span className="font-weight-bold">
                                Task Description: 
                            </span> Function <code>firstJust</code> takes two arguments: a list of <code>Maybe a</code>'s and a default value. It returns the first element from the list that is a <code>Just</code> or the default, if no such element exists.
                        </p>
                    </div>
                    <div>
                        <h6 className="font-weight-bold">Different Search Modes</h6>
                        <ul>
                            <li>{searchByTypeOnly}</li>
                            <li>{searchByExampleOnly}</li>
                            <li>{searchByTypeAndExamples}</li>
                        </ul>
                    </div>
                </div>
            </Card>
        );
    }
    return <></>;
};

const ExampleSearchCard = connect(mapStateToProps, mapDispatchToProps)(ConnectedExampleSearchCard);

export default ExampleSearchCard;