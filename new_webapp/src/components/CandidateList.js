import React from "react";
import { connect } from "react-redux";
import UsageTable from "./UsageTable";
import { BounceLoader } from "react-spinners";
import Collapsible from "react-collapsible";
import { Card } from "react-bootstrap";

const mapStateToProps = state => {
    return {
        candidates: state.candidates.results,
        isFetching: state.candidates.isFetching,
        numArgs: state.spec.numArgs,
    };
}

const CandidateListBase = ({candidates, numArgs, isFetching}) => (
    <div>
        {candidates.map(({code, examples, candidateId}, idx) => {
            const header = (
                <Card.Header>
                    {idx + 1}:
                    <span>Candidate: <code>{code}</code></span>
                </Card.Header>
            );
            return (
                <Card key={idx}>
                    <Collapsible
                        open={true}
                        trigger={header}>
                            <Card.Body>
                                <UsageTable
                                    candidateId={candidateId}
                                    code={code}
                                    rows={examples}
                                    numColumns={numArgs + 1}
                                />
                            </Card.Body>
                    </Collapsible>
                </Card>
            );
            })
        }
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