// @flow
import React from 'react';
import Router from 'react-router-dom/BrowserRouter';
import Route from 'react-router/Route';

import './App.css';

import Navigation from './components/Navigation';
import Home from './components/Home';
import Graph from './components/Graph';
import About from './components/About';

import history from './history';

const App = () => {
  return (
    <Router history={history}>
      <div className="App">

        <Navigation />

        <div className="App-container">
          <Route exact path="/" component={Home} />
          <Route path="/graph/:view*" component={Graph} />
          <Route path="/about" component={About} />
        </div>
      </div>
    </Router>
  );
};

export default App;
