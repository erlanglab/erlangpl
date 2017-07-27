// @flow
import { createStore, applyMiddleware } from 'redux';
import { routerMiddleware } from 'react-router-redux';
import createHistory from 'history/createBrowserHistory';
// import throttle from 'lodash/throttle';

// import { loadState, saveState } from './localStorage';
import rootReducer from './reducer';

export const history = createHistory();

const middleware = routerMiddleware(history);

// NOTE: We're loading state from localStorage
// it's returning cached state or undefined so
// reducers can initialize new state if cached was undefined
// const preservedState = loadState();

const store = createStore(
  rootReducer,
  null, // preservedState,
  applyMiddleware(middleware),
  window.__REDUX_DEVTOOLS_EXTENSION__ && window.__REDUX_DEVTOOLS_EXTENSION__()
);

// store.subscribe(throttle(() => saveState(store.getState()), 1000));

export default store;
