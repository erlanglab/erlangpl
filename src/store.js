// @flow
import { createStore } from 'redux';

import rootReducer from './reducer';

const INITIAL_STATE = {};

const store = createStore(
  rootReducer,
  INITIAL_STATE,
  window.__REDUX_DEVTOOLS_EXTENSION__ &&
    window.__REDUX_DEVTOOLS_EXTENSION__() /*, middleware */
);

import createHistory from 'history/createBrowserHistory';
import { syncHistoryWithStore } from 'react-router-redux';

export const create = (store: mixed) =>
  syncHistoryWithStore(createHistory(), store);

export const history = create(store);

// TODO (baransu) initial data dispatch?

export default store;
