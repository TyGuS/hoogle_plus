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
    const {abort, ready} = abortableFetch(ROUTE, fetchOpts);
    return {
        abort,
        ready: ready.then(handleFetch),
    };
}

const getCodeCandidates = ({query, examples}, cb) => {
    const ROUTE = baseRoute + "search/type";

    let data = {
        typeSignature: query,
        facts: examples || []
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
    return {
        abort,
        ready: ready
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
                        console.log("convertedValue", convertedValue);
                        convertedValue.trim().split("\n").forEach(jsonStr => {
                            try {
                                const jsonBlob = JSON.parse(jsonStr);
                                onIncrementalResponse(jsonBlob);
                                console.log("convertedValue sent:", jsonBlob);
                                msgQueue = "";
                            } catch (error) {
                                console.log("remaining json", jsonStr);
                                msgQueue = msgQueue.concat(jsonStr);
                                console.log("new message queue", msgQueue);
                                // console.error("convertedValue error", error);
                            }
                        })
                        controller.enqueue(value);
                        return pump();
                        });
                    }}
                });
            })
            .then(stream => new Response(stream))
            .then(response => response.text())
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

function abortableFetch(request, opts) {
    const controller = new AbortController();
    const signal = controller.signal;

    return {
      abort: () => controller.abort(),
      ready: fetch(request, { ...opts, signal })
    };
  }

export default {
    getCodeCandidates,
    getTypeCandidates,
    sendStopSignal
};
