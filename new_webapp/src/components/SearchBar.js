import React, {Component } from "react";
import {connect} from "react-redux";
import {setSearchType} from "../actions/index";
import ExampleTable from "./ExampleTable";
import { TypeSelection } from "./TypeSelection";
import { Button, InputGroup, FormControl } from "react-bootstrap";

const mapDispatchToProps = (dispatch) => {
    return {
        setSearchType: searchTerm => setSearchType(searchTerm)(dispatch)
    }
}

const mapStateToProps = (state) => {
    return {
        searchType: state.spec.searchType,
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
        this.props.setSearchType({query: this.state.value});
    }

    render() {
        return (
            <div>
            <TypeSelection/>
            <div className="container">
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
                <div className="row justify-content-center">
                    <div className="col">
                        <div>
                            Example Specifications:
                        </div>
                        <ExampleTable/>
                    </div>
                </div>
                <Button onClick={this.handleSubmit}>
                    Search
                </Button>
            </div>
            </div>
        );
    }
}

const SearchBar = connect(mapStateToProps, mapDispatchToProps)(ConnectedSearchBar);

export default SearchBar;