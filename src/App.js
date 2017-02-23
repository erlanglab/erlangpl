// @flow
import React from 'react';
import Router from 'react-router-dom/BrowserRouter';
import Route from 'react-router/Route';
import { Provider } from 'react-redux';

import './App.css';

import Navigation from './components/Navigation';
import Home from './components/Home';
import Page from './components/Page';
import About from './components/About';

import store, { history } from './store';

const App = () => {
  return (
    <Provider store={store}>
      <Router history={history}>
        <div className="App">

          <Navigation />

          <div className="App-container">
            <Route exact path="/" component={Home} />
            <Route path="/page" component={Page} />
            <Route path="/about" component={About} />
          </div>
        </div>
      </Router>
    </Provider>
  );
};

export default App;
