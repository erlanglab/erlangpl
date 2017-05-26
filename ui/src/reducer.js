// @flow
import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import core from './core';

//plugins
import eplDashboard from './plugins/epl-dashboard';
import eplSupTree from './plugins/epl-sup-tree';
import eplVizceral from './plugins/epl-vizceral';
import eplETS from './plugins/epl-ets';

import plugins from './plugins';

const reducers = plugins.reduce((acc, plugin) => {
  if (plugin.reducer && plugin.name) {
    return {
      ...acc,
      [plugin.name]: plugin.reducer
    };
  }
  console.warn(`Could not register reducer for ${plugin.name}`);
  return acc;
}, {});

export default combineReducers({
  core: core.reducer,
  eplVizceral: eplVizceral.reducer,
  eplETS: eplETS.reducer,
  eplDashboard: eplDashboard.reducer,
  eplSupTree: eplSupTree.reducer,
  ...reducers,
  routing: routerReducer
});
