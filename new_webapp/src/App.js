import React from 'react';
import logo from './logo.svg';
import './App.css';
import CandidateList from './components/CandidateList';
import SearchBar from './components/SearchBar';
import {TopBar} from './components/TopBar';


function App() {
  return (
    <div className="App container">
      <TopBar/>
      <div className="row justify-content-center">
        <div className="col-10">
          <SearchBar />
          <br/>
          <hr/>
          <br/>
          <CandidateList className="left" />
        </div>
        {/* <div className="right col-2">sidebar placeholder</div> */}
      </div>
    </div>
  );
}
export default App;
