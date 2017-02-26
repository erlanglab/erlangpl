// @flow
import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import traffic from './traffic';
import systemInfo from './systemInfo';
import systemOverview from './systemOverview';
import connection from './connection';

const rootReducer = combineReducers({
  traffic,
  systemInfo,
  systemOverview,
  connection,
  routing: routerReducer
});

export default rootReducer;
