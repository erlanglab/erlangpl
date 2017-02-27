// @flow
import React from 'react';
import { render } from 'react-dom';
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
import { updateTrafficData, updateTrafficView } from './actions/traffic';
import history from './history';

const view = syncGraphViewWithHistory(history.location);
store.dispatch(updateTrafficView(view));

// TODO (baransu) remove dispatching dummy data
import sampleData from './sample_data.json';
store.dispatch(updateTrafficData(sampleData));

import { updateSystemInfo } from './actions/systemInfo';

on('system-info', data => {
  const d = humps.camelizeKeys(data);
  store.dispatch(updateSystemInfo(humps.camelizeKeys(d)));
});

on('system-init', data => {
  const d = humps.camelizeKeys(data);
  store.dispatch(updateSystemInfo(humps.camelizeKeys(d)));
});

render(<App store={store} />, document.getElementById('root'));
