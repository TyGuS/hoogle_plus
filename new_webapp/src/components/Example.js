import React from "react";
import { addFact } from "../actions";
import { connect } from "react-redux";

const mapDispatchToProps = (dispatch) => {
    return {
        addFact: (example) => dispatch(addFact(example))
    };
}

const ExampleBase = ({ex, addFact}) => (
    <div>
    {ex.map((e, idx) => (
        <div key={idx}>
            <code>"{e}"</code>
        </div>
    ))}
    <button onClick={addFact.bind(null, ex)}>add fact</button>
    <br/>
    </div>
);

const Example = connect(null, mapDispatchToProps)(ExampleBase);

export default Example;