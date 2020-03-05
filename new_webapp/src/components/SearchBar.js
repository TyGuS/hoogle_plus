import _ from "underscore";
import React, {Component } from "react";
import {connect} from "react-redux";
import {setSearchType, getTypesFromExamples} from "../actions/index";
import ExampleTable from "./ExampleTable";
import { TypeSelection } from "./TypeSelection";
import { Button, InputGroup, FormControl, Form } from "react-bootstrap";
import { getDefaultFeatures } from "../utilities/featureManager";

const mapDispatchToProps = (dispatch) => {
    return {
        setSearchType: ({query, examples}) => setSearchType({query, examples})(dispatch),
        getTypesFromExamples: usages => getTypesFromExamples(usages)(dispatch),
    }
}

const mapStateToProps = (state) => {
    return {
        searchType: state.spec.searchType,
        exampleRows: state.spec.rows,
    }
};

class ConnectedSearchBar extends Component {
    constructor(props) {
        super(props);
        this.state = {value: ""};
        this.handleChange = this.handleChange.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
    }

    handleChange(event) {
        this.setState({value: event.target.value});
    }

    handleSubmit(event) {
        event.preventDefault();
        const usages = this.props.exampleRows.map(({usage}) => usage);
        if (this.isMissingType()){
            this.props.getTypesFromExamples(usages);
            return;
        }
        this.props.setSearchType({query: this.state.value, examples: usages});
    }

    canSubmit() {
        return (!_.isEmpty(this.props.exampleRows)) || (!this.isMissingType());
    }

    isMissingType() {
        return this.state.value === "";
    }

    render() {
        const {search} = getDefaultFeatures();
        return (
            <div>
            <TypeSelection hidden={!search.permitTypeCandidates} />
            <div className="container">
                <Form onSubmit={this.onSubmit}>
                <div className="row justify-content-center">
                <InputGroup className="mb-3 col-8">
                    <FormControl
                    aria-label="Default"
                    aria-describedby="inputGroup-sizing-default"
                    placeholder="Search by type here"
                    value={this.state.value}
                    onChange={this.handleChange}
                    />
                </InputGroup>
                {/*
                    <InputGroup.Text
                    type="text"
                    name="value"
                    placeholder="search by type here"
                    value={this.state.value}
                    onChange={this.handleChange}
                    className="col-8"
                    /> */}
                </div>
                <div
                    className="row justify-content-center"
                    hidden={!search.permitExamples}>
                    <div className="col">
                        <div>
                            Example Specifications:
                        </div>
                        <ExampleTable/>
                    </div>
                </div>
                <Button
                    disabled={!this.canSubmit()}
                    onClick={this.handleSubmit}
                    type="submit">
                    Search
                </Button>
                </Form>
            </div>

            </div>
        );
    }
}

const SearchBar = connect(mapStateToProps, mapDispatchToProps)(ConnectedSearchBar);

export default SearchBar;