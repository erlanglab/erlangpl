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

import store from './store';
import history from './history';
import { syncGraphViewWithHistory } from './utils';
import { updateGraphData, updateGraphView } from './actions/graph';

const view = syncGraphViewWithHistory(history.location);
store.dispatch(updateGraphView(view));

// TODO (baransu) remove dummy data
import sampleData from './sample_data.json';
store.dispatch(updateGraphData(sampleData));

const App = () => {
  return (
    <Provider store={store}>
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
    </Provider>
  );
};

export default App;
