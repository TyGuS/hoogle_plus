import _ from "underscore";

export const baseRoute = process.env.REACT_APP_DEVELOPMENT ? "http://localhost:5000/" : "/"

export const handleFetch = (data) => {
    return data.json()
        .then((body) => {
            if (!_.isUndefined(body.error)) {
                return Promise.reject({error: body.error});
            }
            const {error, ...rest} = body;
            return {rest};
        });
};