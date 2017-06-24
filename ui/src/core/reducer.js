// @flow

import { combineReducers } from 'redux';

import { clearState } from '../localStorage';
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

const timestamp = (state: number = 0, action: any): number => {
  if (action.type === t.SET_TIMESTAMP) {
    // NOTE: It's sideeffect in reducer. Not nice but not that harmful
    if (action.timestamp !== state) {
      clearState();
    }
    return action.timestamp;
  }
  return state;
};

export default combineReducers({
  connection,
  node,
  timestamp
});
