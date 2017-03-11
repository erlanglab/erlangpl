// @flow
import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import core from './core';

//plugins
import eplDashboard from './plugins/epl-dashboard';
import eplSupTree from './plugins/epl-sup-tree';
import eplVizceral from './plugins/epl-vizceral';

export default combineReducers({
  core: core.reducer,
  eplVizceral: eplVizceral.reducer,
  eplDashboard: eplDashboard.reducer,
  eplSupTree: eplSupTree.reducer,
  routing: routerReducer
});
