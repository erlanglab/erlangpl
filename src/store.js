// @flow
import { createStore } from 'redux';
import { syncHistoryWithStore } from 'react-router-redux';
import createHistory from 'history/createBrowserHistory';

import rootReducer from './reducers';

const INITIAL_STATE = {};
const store = createStore(
  rootReducer,
  INITIAL_STATE,
  window.__REDUX_DEVTOOLS_EXTENSION__ &&
    window.__REDUX_DEVTOOLS_EXTENSION__() /*, middleware */,
);

export const history = syncHistoryWithStore(createHistory(), store);

export default store;
