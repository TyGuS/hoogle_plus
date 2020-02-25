
function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

export const hooglePlusTypeSearch = ({query}) => {
    const mockCandidate = {
        code: "\\arg0 arg1-> catMaybes (listToMaybe arg0) arg1",
        examples: [
            ["z", "2", "zz"],
            ["asdf", "0", ""],
            ["qwop", "-1", "error"],
        ]
    };
    return delay(1000)
        .then(_ => mockCandidate);
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