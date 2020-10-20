import {baseRoute, handleFetch, abortableFetch} from "../utilities/fetches";

const getTypeCandidates = ({id, examples, argNames}, cb) => {
    const ROUTE = baseRoute + "search/example";

    let data = {
        facts: examples || [],
        argNames,
    };

    const fetchOpts = {
        method: 'POST', // or 'PUT'
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(data),
    };
    const {abort, ready} = abortableFetch(ROUTE, fetchOpts);
    return {
        abort,
        ready: ready.then(handleFetch),
    };
}

const getCodeCandidates = ({query, examples, argNames}, cb) => {
    const ROUTE = baseRoute + "search/type";

    let data = {
        typeSignature: query,
        facts: examples || [],
        argNames
    };

    const fetchOpts = {
        method: 'POST', // or 'PUT'
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(data),
    };
    return streamResponse(ROUTE, fetchOpts, (jsonBlob => {
        console.log("onIncrementalResponse", jsonBlob);
        cb(jsonBlob);
    }));
}

// Returns a {abort: () -> (), Promise} of the whole accumulated response, as text
// onIncrementalResponse is called on each chunk received.
const streamResponse = (route, fetchOpts, onIncrementalResponse) => {
    const decoder = new TextDecoder("utf-8");
    var msgQueue = "";
    const {abort, ready} = abortableFetch(route, fetchOpts);
    const readyPromise = ready
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
                        const convertedValue = msgQueue.concat(decoder.decode(value));
                        convertedValue.trim().split("\n").forEach(jsonStr => {
                            try {
                                const jsonBlob = JSON.parse(jsonStr);
                                onIncrementalResponse(jsonBlob);
                                msgQueue = "";
                            } catch (error) {
                                msgQueue = msgQueue.concat(jsonStr);
                            }
                        })
                        controller.enqueue(value);
                        return pump();
                    }).catch(error => {
                        if (error.name && error.name === "AbortError") {
                            return
                        }
                        console.error("readableStream error", error)
                    });
                }}
            });
        })
        .then(stream => new Response(stream))
        .then(response => response.text());
    return {
        abort,
        ready: readyPromise,
    };
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
