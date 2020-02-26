import React from 'react';
import logo from './logo.svg';
import './App.css';
import CandidateList from './components/CandidateList';
import SearchBar from './components/SearchBar';
import {TopBar} from './components/TopBar';
import SampleQueries from './components/SampleQueries';
import ComponentList from './components/ComponentList';


function App() {
  return (
    <div className="App container">
      <TopBar/>
      <div className="row">
        <div className="col-10">
          <SearchBar />
          <br/>
          <hr/>
          <br/>
          <CandidateList className="left" />
        </div>
        <div className="right col-2">sidebar placeholder</div>
      </div>
      <footer className="row footer footer-style text-center">
        <div className="text-center col">
          {footerText}
        </div>
      </footer>
    </div>
  );
}

const footerText = `
Made by Zheng, David, Michael, Joe, Ziteng, Ranjit and Nadia.
`;

export default App;
