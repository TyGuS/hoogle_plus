import { baseRoute } from "../utilities/fetches";

function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

export const typeCheck = ({queryType, usage}) => {
    const route = baseRoute + "typecheck";

    const data = {
        typeName: "",
        term: "",
    }

    const fetchOpts = {
        method: "POST",
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(data),
    }
    return fetch(route, fetchOpts);
}