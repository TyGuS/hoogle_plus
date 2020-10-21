import React from "react";
import _ from "underscore";
import { Button } from "react-bootstrap";
import { exampleToNamedArgs } from "../utilities/args";

const ExampleSearchItem = ({description, queryStr, examples, argNames,
    setSearchType, setExamples, setArgNames, doSearch, getTypesFromExamples }) => {
        // click the search button
        const handleClick = () => {
            console.log(queryStr, examples, argNames);
            // set the field on the UI
            setSearchType({query: queryStr});
            setExamples(exampleToNamedArgs(examples));
            if(!queryStr || queryStr.trim() === "") {
                setArgNames(argNames);
                getTypesFromExamples(examples, argNames);
                return;
            }
            
            doSearch({query: queryStr, examples});
        };

        return (
            <Button 
                variant="link" 
                onClick={handleClick}>{description}
            </Button>
        );
};

export default ExampleSearchItem;