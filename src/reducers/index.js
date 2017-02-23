// @flow
import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import trafficData from './trafficData';

const rootReducer = combineReducers({
  trafficData,
  routing: routerReducer,
});

export default rootReducer;
