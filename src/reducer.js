// @flow
import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import traffic from './traffic';
import eplDashboard from './plugins/epl-dashboard';
import core from './core';

export default combineReducers({
  traffic: traffic.reducer,
  dashboard: eplDashboard.reducer,
  core: core.reducer,
  routing: routerReducer
});
