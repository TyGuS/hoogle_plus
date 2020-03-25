import React from "react";
import _ from "underscore";
import { Button, Card, Badge, OverlayTrigger, Tooltip } from "react-bootstrap";
import Collapsible from "react-collapsible";
import { connect } from "react-redux";
import { BounceLoader } from "react-spinners";
import Highlight from "react-highlight.js";
import UsageTable from "./UsageTable";
import { getMoreExamples } from "../actions";
import { LOADING, DONE, ERROR } from "../constants/fetch-states";
import { getDefaultFeatures } from "../utilities/featureManager";
import { usageToExample } from "../utilities/args";

const mapStateToProps = state => {
    return {
        candidates: state.candidates.results,
        isFetching: state.candidates.isFetching,
        numArgs: state.spec.numArgs,
    };
}

const mapDispatchToProps = dispatch => {
    return {
        getMoreExamples: ({candidateId, code, examples}) => dispatch(getMoreExamples({candidateId, code, examples})),
    }
};

const ConnectedCandidateList = (props) => {
    const {candidates, numArgs, isFetching, getMoreExamples} = props;
    const {results: resultsFeatures} = getDefaultFeatures();

    // code: str; docs: [{doc, name, signature}]
    const addDocs = (code, docs) => {
        let docLookup = {};
        _.each(docs, doc => {
            docLookup[doc.name] = doc;
        });
        const wordLike = code.split(/\b/);
        return wordLike.map((word, idx) => {
            const wordSpan = (<span key={idx}>{word}</span>);
            if (! (word in docLookup)) {
                return wordSpan
            }
            const docLine = docLookup[word].doc.split("\n").map((line, idx) => (
                <div key={idx} className="doc-line">{line}</div>));
            const toolTip = (
                <Tooltip>
                    <h6>{word} :: {docLookup[word].signature}</h6>
                    {docLine}
                </Tooltip>
            );
            return (
                <OverlayTrigger
                    key={idx}
                    placement="top"
                    delay={{ show: 150, hide: 200 }}
                    overlay={toolTip}
                >
                {wordSpan}
                </OverlayTrigger>
            );
        });
    }

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
                        {addDocs(code, docs)}
                    </Card.Header>
                );
                const handleClick = () => getMoreExamples({candidateId, code, examples});
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
                                    <div className="col-12">
                                    {resultsFeatures.permitExamples ?
                                    (<UsageTable
                                        candidateId={candidateId}
                                        code={code}
                                        rows={examples}
                                        numColumns={numArgs + 1}
                                    />) : (<></>)}
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