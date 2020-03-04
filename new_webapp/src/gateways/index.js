
import _ from "underscore";
import {v4} from "uuid";
import { usageToId } from "../utilities/args";
export { default as Query } from "./query";

function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

// {id: uuid; examples:[[str]]} -> Promise
export const hooglePlusExampleSearch = ({id, examples}) => {
    const mockUsage = {
        typeCandidates: [
            "a -> b -> a",
            "[a] -> Int -> [a]",
            "String -> Int -> String",
        ]
    };
    const mockError = {
        error: "unstructuredErrorMessage"
    }
    return delay(1000)
        .then(_ => {
        if(Math.random() > 0){
            return mockUsage;
        } else {
            return mockError;
        }});
}

// {code: str, args: [str]} -> Promise
export const ghciUsage = ({code, args}) => {
    const mockUsage = {
        result: "someResult"
    };
    const mockError = {
        error: "unstructuredErrorMessage"
    }
    return delay(1000)
        .then(_ => {
            if(Math.random() > 0.5){
                return mockUsage;
            } else {
                return mockError;
            }});
};

export const ghciTypeCheck = ({queryType, usage}) => {
    return delay(1000)
        .then(_ => {return {}});
}

export const hooglePlusMoreExamples = ({code, usages, queryType}) => {
    const mockExamples = [
        ["z", "3", "zzz"],
        ["z", "0", ""]
    ];
    return delay(1000)
        .then(_ => {return {examples: mockExamples}});
}