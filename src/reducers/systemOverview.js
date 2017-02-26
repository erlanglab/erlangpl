// @flow
import { UPDATE_SYSTEM_INFO } from '../actions/systemInfo';

export const INITIAL_STATE = {
  receive: [],
  memoryTotal: [],
};

type State = {
  receive: Array<{ count: string, sizes: string }>,
  memoryTotal: Array<string>,
};

const systemOverview = (state: State = INITIAL_STATE, action: any) => {
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
