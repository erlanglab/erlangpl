// @flow
import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import graph from './graph';

const rootReducer = combineReducers({
  graph,
  routing: routerReducer,
});

export default rootReducer;
