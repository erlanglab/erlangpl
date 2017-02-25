// @flow
import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import graph from './graph';
import systemInfo from './systemInfo';

const rootReducer = combineReducers({
  graph,
  systemInfo,
  routing: routerReducer,
});

export default rootReducer;
