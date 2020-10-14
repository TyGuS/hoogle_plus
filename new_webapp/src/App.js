import React from 'react';
import './style/App.scss';
import CandidateList from './components/CandidateList';
import SearchBar from './components/SearchBar';
import {TopBar} from './components/TopBar';

function App() {
  return (
    <div className="App container">
      <TopBar/>
      <div className="container">
        <div className=" card py-3">
          <SearchBar />
        </div>
      </div>
      <CandidateList className="left" />
    </div>
  );
}
export default App;
