// @flow
import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import traffic from './traffic';
import systemInfo from './systemInfo';

const rootReducer = combineReducers({
  traffic,
  systemInfo,
  routing: routerReducer,
});

export default rootReducer;
