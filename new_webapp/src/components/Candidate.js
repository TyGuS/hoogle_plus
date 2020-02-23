import React from "react";
import Example from "./Example";

const Candidate = ({idx, code, examples}) => (
    <li>
        <div>{idx}: Candidate: <code>{code}</code></div>
        <div>{examples.map((e, idx) => (
            <div key={idx}>
                <Example ex={e}/>
            </div>
        ))}</div>
    </li>
);

export default Candidate