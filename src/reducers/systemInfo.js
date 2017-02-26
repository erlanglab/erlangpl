// @flow

import { UPDATE_SYSTEM_INFO } from '../actions/systemInfo';

export const INITIAL_STATE = {
  nodeName: 'undefined',
};

const systemInfo = (state: any = INITIAL_STATE, action: any) => {
  if (action.type === UPDATE_SYSTEM_INFO) {
    return {
      ...state,
      ...action.info,
    };
  }
  return state;
};

export default systemInfo;
