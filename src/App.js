// @flow
import React from 'react';
import Router from 'react-router-dom/BrowserRouter';
import Route from 'react-router/Route';

import './App.css';

import Navigation from './components/Navigation';
import Home from './components/Home';
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
          <Route exact path="/" component={Home} />
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
