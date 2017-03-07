// @flow
import * as t from './actionTypes';

const reducer = (state: any = {}, action: any) => {
  if (action.type === t.UPDATE_APPS_INFO) {
    return action.data;
  }
  return state;
};

export default reducer;
