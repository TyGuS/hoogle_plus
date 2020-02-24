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
    <div className="App">
      <TopBar></TopBar>
      <SearchBar></SearchBar>
      {/* <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <p>
          Edit <code>src/App.js</code> and save to reload
        </p>
        <a
          className="App-link"
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
      </header> */}
      <CandidateList></CandidateList>
      <SampleQueries></SampleQueries>
      <ComponentList></ComponentList>
    </div>
  );
}

export default App;
