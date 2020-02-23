import React from 'react';
import logo from './logo.svg';
import './App.css';
import CandidateList from './components/CandidateList';
import SearchBar from './components/SearchBar';
import {TopBar} from './components/TopBar';


function App() {
  return (
    <div className="App">
      <TopBar/>
      <SearchBar/>
      <CandidateList/>
    </div>
  );
}

export default App;
