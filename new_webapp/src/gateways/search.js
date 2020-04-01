import {v4} from "uuid";
import { inputsToId } from "../utilities/args";
import {baseRoute, handleFetch} from "../utilities/fetches";
import { DONE } from "../constants/fetch-states";

const getTypeCandidates = ({id, examples}, cb) => {
    const ROUTE = baseRoute + "search/example";

    let data = {
        facts: examples || []
    };

    const fetchOpts = {
        method: 'POST', // or 'PUT'
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(data),
    };
    return fetch(ROUTE, fetchOpts)
        .then(handleFetch);
}

const getCodeCandidates = ({query, examples}, cb) => {
    const ROUTE = baseRoute + "search/type";

    let data = {
        typeSignature: query,
        facts: examples || []
    };

    const convertToState = ({id, candidate, examples, docs, error}) => {
        const newResults = {
                candidateId: v4(),
                code: candidate,
                docs,
                examplesStatus: DONE,
                examples: examples.map(({inputs, output}) => {
                    return {
                        id: inputsToId(inputs),
                        inputs,
                        output,
                        usage: inputs.concat(output),
                        isLoading: false,
                    }}),
            };
        return {
            error,
            queryId: id,
            docs,
            result: newResults
        };
    };

    const fetchOpts = {
        method: 'POST', // or 'PUT'
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(data),
    };
    return streamResponse(ROUTE, fetchOpts, (jsonBlob => {
        console.log("onIncrementalResponse", jsonBlob);
        // const newState = convertToState(jsonBlob);
        // cb(newState);
        cb(jsonBlob);
    }));
}

// Returns a promise of the whole accumulated response, as text
// onIncrementalResponse is called on each chunk received.
const streamResponse = (route, fetchOpts, onIncrementalResponse) => {
    const decoder = new TextDecoder("utf-8")

    return fetch(route, fetchOpts)
        .then(response => response.body)
        .then(body => {
            const reader = body.getReader();
            return new ReadableStream({
                start(controller) {
                return pump();
                function pump() {
                    return reader.read().then(({ done, value }) => {
                    // When no more data needs to be consumed, close the stream
                    if (done) {
                        controller.close();
                        return;
                    }
                    // Enqueue the next data chunk into our target stream
                    const convertedValue = decoder.decode(value);
                    console.log("convertedValue", convertedValue);
                    convertedValue.trim().split("\n").forEach(jsonStr => {
                        try {
                            const jsonBlob = JSON.parse(jsonStr);
                            onIncrementalResponse(jsonBlob);
                            console.log("convertedValue sent:", jsonBlob);
                        } catch (error) {
                            console.error("convertedValue error", error);
                        }
                    })
                    controller.enqueue(value);
                    return pump();
                    });
                }}
            });
        })
        .then(stream => new Response(stream))
        .then(response => response.text());
}

const sendStopSignal = ({id}) => {
    const ROUTE = baseRoute + "stop";
    let data = {id};
    const fetchOpts = {
        method: 'POST', // or 'PUT'
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(data),
    };
    return fetch(ROUTE, fetchOpts)
        .then(response => response.text());
}

export default {
    getCodeCandidates,
    getTypeCandidates,
    sendStopSignal
};
