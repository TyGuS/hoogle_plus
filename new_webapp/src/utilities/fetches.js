// Launch with:
// REACT_APP_DEVELOPMENT_HOST="<hostname>:<port>" yarn start
// to point the backend wherever you need it to
const devBase = "http://" + (process.env.REACT_APP_DEVELOPMENT_HOST || "localhost:5000") + "/"
export const baseRoute = process.env.REACT_APP_DEVELOPMENT ? devBase : "https://18.220.20.36:5000/"

export const handleFetch = (data) => {
    return data.json()
        .then((body) => {
            if (body.error) {
                console.log("handleFetch error:", body.error);
                return Promise.reject({error: body.error});
            }
            const {error, ...rest} = body;
            return {...rest};
        });
};

export function abortableFetch(request, opts) {
    const controller = new AbortController();
    const signal = controller.signal;

    return {
      abort: () => controller.abort(),
      ready: fetch(request, { ...opts, signal })
    };
}
