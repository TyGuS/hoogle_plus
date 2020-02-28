
function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

export const hooglePlusTypeSearch = ({query, examples}) => {
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
    return delay(1000)
        .then(_ => mockCandidate);
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
        if(Math.random() > 0.5){
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