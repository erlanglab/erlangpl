// @flow
import React from 'react';
import { render } from 'react-dom';
import { Provider } from 'react-redux';
import humps from 'humps';

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

import { updateSystemInfo } from './actions/systemInfo';

on('system-info', data => {
  console.log('system info', data);
});

on('system-init', data => {
  const d = humps.camelizeKeys(data);
  console.log('system-init', d);
  store.dispatch(updateSystemInfo(humps.camelizeKeys(d)));
});

render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('root'),
);
