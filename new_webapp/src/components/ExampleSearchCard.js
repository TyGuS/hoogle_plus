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
            name = "firstJust"
            description = "d:a -> xs:[Maybe a] -> a"
            queryStr = "d:a -> xs:[Maybe a] -> a"
            examples = {[]}
            argNames = {["d", "xs"]}
            setSearchType = {setSearchType}
            setExamples = {setExamples}
            setArgNames = {setArgNames}
            doSearch = {doSearch}
            getTypesFromExamples = {getTypesFromExamples}
        />
    );

    const searchByExampleOnly = (
        <ExampleSearchItem
            name = "dedup"
            description = '"aaaabbbab" -> "abab"; [1,1,1,2,2,3] -> [1,2,3]'
            queryStr = ""
            examples = {
                [{
                    id: "ex-1",
                    inputs: ["\"aaaabbbab\""],
                    output: "\"abab\""
                },
                {
                    id: "ex-2",
                    inputs: ["[1,1,1,2,2,3]"],
                    output: "[1,2,3]"
                }]
            }
            argNames = {["xs"]}
            setSearchType = {setSearchType}
            setExamples = {setExamples}
            setArgNames = {setArgNames}
            doSearch = {doSearch}
            getTypesFromExamples = {getTypesFromExamples}
        />
    );

    const searchByTypeAndExamples = (
        <ExampleSearchItem
            name = "concatNTimes"
            description = 'xs:[a] -> n:Int -> [a]; [1,2,3] -> 2 -> [1,2,3,1,2,3]; "abc" -> 3 -> "abcabcabc"'
            queryStr = 'xs:[a] -> n:Int -> [a]'
            examples = {
                [{
                    id: "ex-1",
                    inputs: ["[1,2,3]", "2"],
                    output: "[1,2,3,1,2,3]"
                },
                {
                    id: "ex-2",
                    inputs: ["\"abc\"", "3"],
                    output: "\"abcabcabc\""
                }]
            }
            argNames = {["xs", "n"]}
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
                    <h6 className="font-weight-bold">
                        {title}
                    </h6>
                    <div>
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