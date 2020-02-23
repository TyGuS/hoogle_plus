import React, {Component } from "react";
import {connect} from "react-redux";
import {sendSearch} from "../actions/index";
import FactTable from "./FactTable";

const mapDispatchToProps = (dispatch) => {
    return {
        sendSearch: searchTerm => sendSearch(searchTerm)(dispatch)
    }
}

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
        const {query} = this.state.value;
        this.props.sendSearch({query});
    }

    render() {
        return (
            <div>
            <form onSubmit={this.handleSubmit}>
                <input type="text" name="value" value={this.state.value} onChange={this.handleChange}/>
                <input type="submit" value="Search" />
            </form>
            <FactTable/>
            </div>
        );
    }
}

const SearchBar = connect(null, mapDispatchToProps)(ConnectedSearchBar);

export default SearchBar;