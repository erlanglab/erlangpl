// @flow
import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import core from './core';

//plugins
import eplDashboard from './plugins/epl-dashboard';
import eplSupTree from './plugins/epl-sup-tree';
import traffic from './traffic';

export default combineReducers({
  core: core.reducer,
  traffic: traffic.reducer,
  dashboard: eplDashboard.reducer,
  eplSupTree: eplSupTree.reducer,
  routing: routerReducer
});
