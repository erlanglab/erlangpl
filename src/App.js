// @flow
import React from 'react';
import { BrowserRouter as Router, Route, Redirect } from 'react-router-dom';

import './App.css';

import Navigation from './components/Navigation';
import Index from './components/Index';
import Traffic from './components/Traffic';
import Messages from './components/Messages';
import About from './components/About';
import Footer from './components/Footer';

import history from './history';

const App = () => {
  return (
    <Router history={history}>
      <div className="App">

        <Navigation />

        <div className="App-container">
          <Route exact path="/" render={() => <Redirect to="/home" />} />
          <Route path="/home/:subRoute*" component={Index} />
          <Route path="/messages" component={Messages} />
          <Route path="/traffic/:view*" component={Traffic} />
          <Route path="/about" component={About} />
        </div>
        <Footer />
      </div>
    </Router>
  );
};

export default App;
