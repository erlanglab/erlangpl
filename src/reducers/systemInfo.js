// @flow

import { UPDATE_SYSTEM_INFO } from '../actions/systemInfo';

export const INITIAL_STATE = {
  nodeName: 'undefined',
  receive: [],
};

const systemInfo = (state: any = INITIAL_STATE, action: any) => {
  if (action.type === UPDATE_SYSTEM_INFO) {
    return {
      ...state,
      ...action.info,
      receive: state.receive
        .concat(action.info.receive)
        .filter(a => a !== undefined),
    };
  }
  return state;
};

export default systemInfo;
