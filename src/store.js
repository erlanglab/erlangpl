// @flow
import { createStore } from 'redux';
import rootReducer from './reducers';

const INITIAL_STATE = {};

const store = createStore(rootReducer, INITIAL_STATE /*, middleware */);

export default store;
