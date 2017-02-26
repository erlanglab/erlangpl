// @flow

import { CONNECTION_OPEN, CONNECTION_CLOSE } from '../actions/connection';

export const INITIAL_STATE = 'disconnected';

type State = 'connected' | 'disconnected';

const connection = (state: State = INITIAL_STATE, action: any) => {
  if (action.type === CONNECTION_OPEN) {
    return 'connected';
  }

  if (action.type === CONNECTION_CLOSE) {
    return 'disconnected';
  }

  return state;
};

export default connection;
