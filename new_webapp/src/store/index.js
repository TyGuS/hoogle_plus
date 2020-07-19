import { createStore, applyMiddleware } from "redux";
import thunk from "redux-thunk";
import { createLogger } from "redux-logger";
import rootReducer from "../reducers/index";
import { composeWithDevTools } from "redux-devtools-extension";

const loggerMiddleware = createLogger()


const store = createStore(
    rootReducer,
    composeWithDevTools(
        applyMiddleware(
            thunk,
            loggerMiddleware
        )
    ),
);


export default store;