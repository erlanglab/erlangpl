// @flow

import { combineReducers } from 'redux';

import * as t from './actionTypes';

export const INITIAL_STATE = 'disconnected';

type State = 'connected' | 'disconnected';

const connection = (state: State = INITIAL_STATE, action: any): State => {
  if (action.type === t.CONNECTION_OPEN) {
    return 'connected';
  }

  if (action.type === t.CONNECTION_CLOSE) {
    return 'disconnected';
  }

  return state;
};

const node = (state: string = '', action: any): string => {
  if (action.type === t.SET_NODE) {
    return action.node;
  }
  return state;
};

export default combineReducers({
  connection,
  node
});
