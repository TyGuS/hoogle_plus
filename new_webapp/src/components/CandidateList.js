import React from "react";
import { connect } from "react-redux";
import Example from "./Example";
import UsageTable from "./UsageTable";

const mapStateToProps = state => {
    return {
        candidates: state.candidates,
        numArgs: state.numArgs,
    };
}

const CandidateListBase = ({candidates, numArgs}) => (
    <div>
        {candidates.map(({code, examples}, idx) => (
            <div key={idx}>
                <div>{idx + 1}: Candidate: <code>{code}</code></div>
                <UsageTable
                    rows={examples}
                    numColumns={numArgs + 1}
                />
            </div>
        ))}
    </div>
);

const CandidateList = connect(mapStateToProps)(CandidateListBase);

export default CandidateList;