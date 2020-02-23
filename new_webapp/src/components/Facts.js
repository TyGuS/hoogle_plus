import React from "react";
import { connect } from "react-redux";

const mapDispatchToProps = (dispath) => {};

const mapStateToProps = (state) => {
    return {
        facts: state.facts
    }
};

const FactsBase = ({facts}) => (
    <div className="facts">
        {facts.map((f, idx) => (
            <div className="factRow" key={idx}>
                {f.map((fact_item, f_fix) => (
                    <div key={f_fix}>
                        <code>"{fact_item}"</code>
                    </div>
                ))}
            </div>
        ))}
    </div>
);

const Facts = connect(mapStateToProps, mapDispatchToProps)(FactsBase);

export default Facts;