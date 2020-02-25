import React from "react";
import { connect } from "react-redux";
import UsageTable from "./UsageTable";
import { BounceLoader } from "react-spinners";
import Collapsible from "react-collapsible";

const mapStateToProps = state => {
    return {
        candidates: state.candidates.results,
        isFetching: state.candidates.isFetching,
        numArgs: state.spec.numArgs,
    };
}

const CandidateListBase = ({candidates, numArgs, isFetching}) => (
    <div>
        {candidates.map(({code, examples, candidateId}, idx) => (
            <div key={idx}>
                <div>{idx + 1}: Candidate: <code>{code}</code></div>
                <Collapsible trigger="Open usage table">
                    <UsageTable
                        candidateId={candidateId}
                        code={code}
                        rows={examples}
                        numColumns={numArgs + 1}
                    />
                </Collapsible>
            </div>
        ))}
        {/* https://www.npmjs.com/package/react-spinners */}
        <div className="container">
            <div className="row justify-content-center">
                <BounceLoader loading={isFetching}/>
            </div>
        </div>
    </div>
);

const CandidateList = connect(mapStateToProps)(CandidateListBase);

export default CandidateList;