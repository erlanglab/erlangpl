// @flow
import * as t from './actionTypes';

const reducer = (state = { kernel: {} }, action) => {
  if (action.type === t.UPDATE_APPS_INFO) {
    return action.data;
  }
  return state;
};

export default reducer;
