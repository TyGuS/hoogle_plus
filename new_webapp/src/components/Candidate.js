import React from "react";

const Candidate = ({idx, code, examples}) => (
    <li>
        <div>{idx}: Candidate: <code>{code}</code></div>
        <div>{examples.map((e, idx) => (
            <div key={idx}> <code>"{e}"</code>
            </div>
        ))}</div>
    </li>
);

export default Candidate