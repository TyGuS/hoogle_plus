
import _ from "underscore";

function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

const baseRoute = process.env.REACT_APP_DEVELOPMENT ? "http://localhost:5000/" : "/"

export const hooglePlusTypeSearch = ({query, examples}) => {
    const ROUTE = baseRoute + "search/type";

    let data = {
        typeSignature: query,
        facts: examples || []
    };

    const mockCandidate = {
        code: "\\arg0 arg1-> catMaybes (listToMaybe arg0) arg1",
        examples: [
            {   id: "34",
                usage: ["z", "2", "zz"],
                isLoading: false,
            },
            {   id: "35",
                usage: ["z", "5", "zzzzz"],
                isLoading: false,
            },
            {   id: "36",
                usage: ["abc", "-1", "error"],
                isLoading: false,
            },
        ]
    };
    // return delay(1000)
    //     .then(_ => mockCandidate);

    return fetch(ROUTE, {
        method: 'POST', // or 'PUT'
        mode: 'no-cors',
        headers: {
        'Content-Type': 'application/json',
        'Access-Control-Allow-Origin': '*',
        },
        body: JSON.stringify(data),
    }).then(response => {return response.json()})
    .catch(error => {console.log(error); return mockCandidate;});
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