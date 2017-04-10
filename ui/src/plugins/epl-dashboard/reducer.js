// @flow
import { combineReducers } from 'redux';

import * as t from './actionTypes';

export const INITIAL_STATE_SYSTEM_INFO = {
  nodeName: 'undefined'
};

const systemInfo = (state: any = INITIAL_STATE_SYSTEM_INFO, action: any) => {
  if (action.type === t.UPDATE_SYSTEM_INFO) {
    return {
      ...state,
      ...action.info
    };
  }
  return state;
};

export const INITIAL_STATE_SYSTEM_OVERVIEW = {
  receive: [],
  memoryTotal: [],
  processCount: []
};

type State = {
  receive: Array<{ count: string, sizes: string }>,
  memoryTotal: Array<string>,
  processCount: Array<string>
};

const systemOverview = (
  state: State = INITIAL_STATE_SYSTEM_OVERVIEW,
  action: any
) => {
  if (action.type === t.UPDATE_SYSTEM_INFO) {
    return {
      receive: state.receive
        .concat(action.info.receive)
        .filter(a => a !== undefined),
      memoryTotal: state.memoryTotal
        .concat(action.info.memoryTotal)
        .filter(a => a !== undefined),
      processCount: state.processCount
        .concat(action.info.processCount)
        .filter(a => a !== undefined)
    };
  }

  return state;
};

export default combineReducers({
  systemInfo,
  systemOverview
});
