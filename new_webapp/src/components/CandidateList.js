import React from "react";
import { Button, Card } from "react-bootstrap";
import Collapsible from "react-collapsible";
import { connect } from "react-redux";
import { BounceLoader } from "react-spinners";
import UsageTable from "./UsageTable";
import { getMoreExamples } from "../actions";

const mapStateToProps = state => {
    return {
        candidates: state.candidates.results,
        isFetching: state.candidates.isFetching,
        numArgs: state.spec.numArgs,
    };
}

const mapDispatchToProps = dispatch => {
    return {
        getMoreExamples: ({candidateId, code, usages}) => dispatch(getMoreExamples({candidateId, code, usages})),
    }
};

const ConnectedCandidateList = (props) => {
    const {candidates, numArgs, isFetching, getMoreExamples} = props;
    return (
        <div>
            {candidates.map((result, idx) => {
                const {code, examples, examplesLoading, candidateId} = result;
                const header = (
                    <Card.Header>
                        {idx + 1}:
                        <span>Candidate: <code>{code}</code></span>
                    </Card.Header>
                );
                const usages = examples.map(ex => ex.usage);
                const handleClick = () => getMoreExamples({candidateId, code, usages});
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
                                    <Button
                                        variant="outline-primary"
                                        size="sm"
                                        disabled={examplesLoading}
                                        onClick={examplesLoading ? null : handleClick}>
                                        {examplesLoading ? "Loading..." : "More examples"}
                                    </Button>
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
    )
};

const CandidateList = connect(mapStateToProps, mapDispatchToProps)(ConnectedCandidateList);

export default CandidateList;