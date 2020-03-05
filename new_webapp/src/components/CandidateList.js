import React from "react";
import { Button, Card } from "react-bootstrap";
import Collapsible from "react-collapsible";
import { connect } from "react-redux";
import { BounceLoader } from "react-spinners";
import UsageTable from "./UsageTable";
import { getMoreExamples } from "../actions";
import { LOADING, DONE, ERROR } from "../constants/fetch-states";
import { getDefaultFeatures } from "../utilities/featureManager";

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
    const {results: resultsFeatures} = getDefaultFeatures();
    return (
        <div>
            {candidates.map((result, idx) => {
                const {code, examplesStatus, candidateId} = result;
                const examples = result.examples || [];
                const header = (
                    <Card.Header>
                        {idx + 1}:
                        <span>Candidate: <code>{code}</code></span>
                    </Card.Header>
                );
                const usages = examples.map(ex => ex.usage);
                const handleClick = () => getMoreExamples({candidateId, code, usages});
                const isOpen = examples.length > 0;
                const isLoading = examplesStatus === LOADING;
                const buttonVariant = examplesStatus === ERROR ? "outline-danger" : "outline-primary"
                return (
                    <Card key={idx}>
                        <Collapsible
                            open={isOpen}
                            trigger={header}>
                                <Card.Body>
                                    <UsageTable
                                        candidateId={candidateId}
                                        code={code}
                                        rows={examples}
                                        numColumns={numArgs + 1}
                                    />
                                    {resultsFeatures.enableGetMoreExamples ? (
                                    <Button
                                        variant={buttonVariant}
                                        size="sm"
                                        disabled={isLoading}
                                        onClick={isLoading ? null : handleClick}>
                                        {examplesStatus === LOADING ? "Loading..." :
                                         examplesStatus === DONE ? "More Examples" :
                                         "Error"
                                        }
                                    </Button>) : (<></>)}
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