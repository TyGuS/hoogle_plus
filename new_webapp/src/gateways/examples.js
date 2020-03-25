import {handleFetch, baseRoute} from "../utilities/fetches";

// {typeSignature: str, code: str, args: [str]} -> Promise
export const ghciUsage = ({typeSignature, code, args}) => {
    const route = baseRoute + "example/code";

    const data = {
        typeSignature,
        args,
        candidate: code,
    }

    const fetchOpts = {
        method: "POST",
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(data),
    }
    return fetch(route, fetchOpts)
        .then(handleFetch);
};

export const hooglePlusMoreExamples = ({code, examples, queryType}) => {
    const route = baseRoute + "examples";

    const data = {
        typeSignature: queryType,
        candidate: code,
        examples,
    }

    const fetchOpts = {
        method: "POST",
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(data),
    }
    return fetch(route, fetchOpts).then(handleFetch);
}