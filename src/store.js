// @flow
import { createStore, combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import traffic from './traffic';
import home from './home';
import core from './core';

const rootReducer = combineReducers({
  traffic: traffic.reducer,
  home: home.reducer,
  core: core.reducer,
  routing: routerReducer
});

const INITIAL_STATE = {};

const store = createStore(
  rootReducer,
  INITIAL_STATE,
  window.__REDUX_DEVTOOLS_EXTENSION__ &&
    window.__REDUX_DEVTOOLS_EXTENSION__() /*, middleware */
);

export const history = core.history.create(store);

// TODO (baransu) initial data dispatch?

export default store;
