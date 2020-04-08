import { baseRoute } from "../utilities/fetches";

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