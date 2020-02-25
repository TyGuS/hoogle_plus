import React from "react";
import { connect } from "react-redux";
import UsageTable from "./UsageTable";
import { BounceLoader } from "react-spinners";

const mapStateToProps = state => {
    return {
        candidates: state.candidates.results,
        isFetching: state.candidates.isFetching,
        numArgs: state.numArgs,
    };
}

const CandidateListBase = ({candidates, numArgs, isFetching}) => (
    <div>
        {candidates.map(({code, examples, candidateId}, idx) => (
            <div key={idx}>
                <div>{idx + 1}: Candidate: <code>{code}</code></div>
                <UsageTable
                    candidateId={candidateId}
                    code={code}
                    rows={examples}
                    numColumns={numArgs + 1}
                />
            </div>
        ))}
        {/* https://www.npmjs.com/package/react-spinners */}
        <BounceLoader loading={isFetching}/>
    </div>
);

const CandidateList = connect(mapStateToProps)(CandidateListBase);

export default CandidateList;