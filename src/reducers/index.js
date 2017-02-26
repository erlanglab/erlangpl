// @flow
import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import traffic from './traffic';
import systemInfo from './systemInfo';
import systemOverview from './systemOverview';

const rootReducer = combineReducers({
  traffic,
  systemInfo,
  systemOverview,
  routing: routerReducer,
});

export default rootReducer;
