import React from "react";
import { connect } from "react-redux";
import UsageTable from "./UsageTable";
import { BounceLoader } from "react-spinners";
import Collapsible from "react-collapsible";
import { Accordion, Card, Button } from "react-bootstrap";

const mapStateToProps = state => {
    return {
        candidates: state.candidates.results,
        isFetching: state.candidates.isFetching,
        numArgs: state.spec.numArgs,
    };
}

const CandidateListBase = ({candidates, numArgs, isFetching}) => (
    <div>
        <Accordion>
        {candidates.map(({code, examples, candidateId}, idx) => (
            <Card key={idx}>
                <Card.Header>
                    <Accordion.Toggle
                        as={Button}
                        varaint="link"
                        eventKey={idx}>
                            {idx + 1}:
                    </Accordion.Toggle>
                    <span>Candidate: <code>{code}</code></span>
                </Card.Header>
                <Accordion.Collapse eventKey={idx}>
                    <Card.Body>
                        <UsageTable
                            candidateId={candidateId}
                            code={code}
                            rows={examples}
                            numColumns={numArgs + 1}
                        />
                    </Card.Body>
                </Accordion.Collapse>
            </Card>
        ))}
        </Accordion>
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