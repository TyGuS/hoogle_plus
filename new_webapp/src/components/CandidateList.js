import React from "react";
import { connect } from "react-redux";
import Candidate from "./Candidate";

const mapStateToProps = state => {
    return {
        candidates: state.candidates
    };
}

const CandidateListBase = ({candidates}) => (
    <ul>
        {candidates.map((c, idx) => (
            <Candidate key={idx} idx={idx + 1} {...c}>
            </Candidate>
        ))}
    </ul>
);

const CandidateList = connect(mapStateToProps)(CandidateListBase);

export default CandidateList;