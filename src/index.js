// @flow
import React from 'react';
import { render } from 'react-dom';
import { Provider } from 'react-redux';

import App from './App';
import { on } from './sockets';

// CSS imports
import 'bootstrap/dist/css/bootstrap.css';
import 'bootstrap/dist/css/bootstrap-theme.css';
import 'font-awesome/css/font-awesome.min.css';
import './index.css';

import store from './store';
import { syncGraphViewWithHistory } from './utils';
import { updateGraphData, updateGraphView } from './actions/graph';
import history from './history';

const view = syncGraphViewWithHistory(history.location);
store.dispatch(updateGraphView(view));

// TODO (baransu) remove dispatching dummy data
import sampleData from './sample_data.json';
store.dispatch(updateGraphData(sampleData));

on('system-info', data => {
  console.log('system info', data);
});

on('system-init', data => {
  console.log('system-init', data);
});

render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('root'),
);
