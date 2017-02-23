// @flow
import React from 'react';
import Router from 'react-router-dom/BrowserRouter';
import Route from 'react-router/Route';
import { Provider } from 'react-redux';

import './App.css';

import Navigation from './components/Navigation';
import Home from './components/Home';
import Graph from './components/Graph';
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
            <Route path="/graph" component={Graph} />
            <Route path="/about" component={About} />
          </div>
        </div>
      </Router>
    </Provider>
  );
};

// TODO (baransu) remove dummy data
import sampleTrafficData from './sample_data.json';
import { updateTraffic } from './actions/trafficData';
store.dispatch(updateTraffic(sampleTrafficData));

export default App;
