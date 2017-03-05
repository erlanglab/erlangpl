// @flow
import React from 'react';
import { BrowserRouter as Router, Route, Redirect } from 'react-router-dom';
import { Provider } from 'react-redux';

import './App.css';

import core from './core';
const { Navigation, Footer } = core.components;

import home from './home';
import traffic from './traffic';
import about from './about';

const App = ({ store, history }: { store: mixed, history: mixed }) => {
  return (
    <Provider store={store}>
      <Router history={history}>
        <div className="App">

          <Navigation />

          <div className="App-container">
            <Route exact path="/" render={() => <Redirect to="/home" />} />
            <Route path="/home/:subRoute*" component={home.components.Home} />
            <Route
              path="/traffic/:view*"
              component={traffic.components.Traffic}
            />
            <Route path="/about" component={about.components.About} />
          </div>
          <Footer />
        </div>
      </Router>
    </Provider>
  );
};

export default App;
