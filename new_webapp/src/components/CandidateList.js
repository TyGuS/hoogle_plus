import React from "react";
import { connect } from "react-redux";
import Example from "./Example";
import UsageTable from "./UsageTable";
import { BounceLoader } from "react-spinners";

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
                    code={code}
                    rows={examples}
                    numColumns={numArgs + 1}
                />
            </div>
        ))}
        {/* https://www.npmjs.com/package/react-spinners */}
        <BounceLoader loading={true}/>
    </div>
);

const CandidateList = connect(mapStateToProps)(CandidateListBase);

export default CandidateList;