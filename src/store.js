// @flow
import { createStore, applyMiddleware } from 'redux';
import { routerMiddleware } from 'react-router-redux';
import createHistory from 'history/createBrowserHistory';

import rootReducer from './reducer';

const INITIAL_STATE = {};

export const history = createHistory();

const middleware = routerMiddleware(history);

const store = createStore(
  rootReducer,
  INITIAL_STATE,
  applyMiddleware(middleware),
  window.__REDUX_DEVTOOLS_EXTENSION__ && window.__REDUX_DEVTOOLS_EXTENSION__()
);

export default store;
