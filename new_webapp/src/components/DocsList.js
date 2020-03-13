import React from "react";
import { OverlayTrigger, Tooltip } from "react-bootstrap";
import Highlight from "react-highlight.js";

const Doc = ({doc, name, signature, key}) => {
    const baseDoc = (
        <span>
            <Highlight language="haskell">
                {name} :: {signature}
            </Highlight>
        </span>
    );
    if(!doc) {
        return baseDoc;
    }
    return (
        <>
        <OverlayTrigger
            key={key}
            placement="auto"
            containerpadding="200"
            overlay={
                <Tooltip id="tooltip-top">
                    {doc}
                </Tooltip>
            }
        >
            {baseDoc}
        </OverlayTrigger>{" "}
        </>
    );
}

const DocsList = (props) => {
    const {docs} = props;

    return (
        <div>
            {docs.map(({doc, name, signature}, idx) => (
                <Doc
                    doc={doc}
                    name={name}
                    signature={signature}
                    key={idx}
                />))
            }
        </div>
    );
}

export default DocsList;