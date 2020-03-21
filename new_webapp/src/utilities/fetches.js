import _ from "underscore";

export const baseRoute = process.env.REACT_APP_DEVELOPMENT ? "http://home.michaelbjames.com:5678 /" : "/"

export const handleFetch = (data) => {
    return data.json()
        .then((body) => {
            if (body.error) {
                return Promise.reject({error: body.error});
            }
            const {error, ...rest} = body;
            return {...rest};
        });
};