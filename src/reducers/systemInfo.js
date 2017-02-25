// @flow

import { UPDATE_SYSTEM_INFO } from '../actions/systemInfo';

export const INITIAL_STATE = {
  nodeName: 'undefined',
};

const systemInfo = (state = INITIAL_STATE, action) => {
  if (action.type === UPDATE_SYSTEM_INFO) {
    console.log(action.info);
    return action.info;
  }
  return state;
};

export default systemInfo;
