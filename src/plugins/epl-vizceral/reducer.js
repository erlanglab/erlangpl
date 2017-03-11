// @flow
import * as t from './actionTypes';

export const INITIAL_STATE = {
  data: {
    nodes: [],
    connections: []
  },
  view: []
};

const reducer = (state: any = INITIAL_STATE, action: any) => {
  if (action.type === t.UPDATE_TRAFFIC_DATA) {
    return {
      ...state,
      data: action.data
    };
  }

  if (action.type === t.UPDATE_TRAFFIC_VIEW) {
    return {
      ...state,
      view: action.view
    };
  }

  return state;
};

export default reducer;
