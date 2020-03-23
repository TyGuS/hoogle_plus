import React from "react";
import { Button, Card, Badge } from "react-bootstrap";
import Collapsible from "react-collapsible";
import { connect } from "react-redux";
import { BounceLoader } from "react-spinners";
import Highlight from "react-highlight.js";
import UsageTable from "./UsageTable";
import { getMoreExamples } from "../actions";
import { LOADING, DONE, ERROR } from "../constants/fetch-states";
import { getDefaultFeatures } from "../utilities/featureManager";
import { usageToExample } from "../utilities/args";
import DocsList from "./DocsList";

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
                const {code, examplesStatus, candidateId, errorMessage, docs} = result;
                const examples = result.examples || [];
                const header = (
                    <Card.Header className="candidate-header">
                        <h4>
                            <Badge variant="secondary"
                                className="badge"
                            >
                                {idx + 1}
                            </Badge>
                        </h4>
                        <Highlight language="haskell" className="candidate-code">{code}</Highlight>
                    </Card.Header>
                );
                const usages = examples.map(ex => usageToExample(ex.usage));
                const handleClick = () => getMoreExamples({candidateId, code, usages});
                const isOpen = examples.length > 0 && resultsFeatures.permitExamples;
                const isLoading = examplesStatus === LOADING;
                const buttonVariant = examplesStatus === ERROR ? "outline-danger" : "outline-primary"
                const message = examplesStatus === LOADING ? "Loading..." :
                    examplesStatus === DONE ? "More Examples" :
                    errorMessage;
                return (
                    <Card key={idx}>
                        <Collapsible
                            open={isOpen}
                            triggerDisabled={!resultsFeatures.permitExamples}
                            trigger={header}>
                                <Card.Body>
                                    <div className="row">
                                    <div className="col-10">
                                    {resultsFeatures.permitExamples ?
                                    (<UsageTable
                                        candidateId={candidateId}
                                        code={code}
                                        rows={examples}
                                        numColumns={numArgs + 1}
                                    />) : (<></>)}
                                    </div>
                                    <div className="col-2">
                                        <DocsList docs={docs}/>
                                    </div>
                                    </div>
                                    {resultsFeatures.enableGetMoreExamples ? (
                                    <Button
                                        variant={buttonVariant}
                                        size="sm"
                                        disabled={isLoading}
                                        onClick={isLoading ? null : handleClick}>
                                        {message}
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