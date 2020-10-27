import React from "react";
import _ from "underscore";
import { Button } from "react-bootstrap";
import { exampleToNamedArgs } from "../utilities/args";

const ExampleSearchItem = ({name, description, queryStr, examples, argNames,
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
            <span>
                <span className="btn text no-cursor">{name}:</span>
                <Button
                    variant="link"
                    onClick={handleClick}>
                        <code>{description}</code>
                </Button>
            </span>
        );
};

export default ExampleSearchItem;