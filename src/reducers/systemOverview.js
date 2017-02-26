// @flow
import { UPDATE_SYSTEM_INFO } from '../actions/systemInfo';

export const INITIAL_STATE = {
  receive: [],
  memoryTotal: [],
};

const systemOverview = (state = INITIAL_STATE, action) => {
  if (action.type === UPDATE_SYSTEM_INFO) {
    return {
      receive: state.receive
        .concat(action.info.receive)
        .filter(a => a !== undefined),
      memoryTotal: state.memoryTotal
        .concat(action.info.memoryTotal)
        .filter(a => a !== undefined),
    };
  }

  return state;
};

export default systemOverview;
