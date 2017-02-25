// @flow
import { UPDATE_TRAFFIC_DATA, UPDATE_TRAFFIC_VIEW } from '../actions/traffic';

export const INITIAL_STATE = {
  data: {
    nodes: [],
    connections: [],
  },
  view: [],
};

const traffic = (state: any = INITIAL_STATE, action: any) => {
  if (action.type === UPDATE_TRAFFIC_DATA) {
    return {
      ...state,
      data: action.data,
    };
  }

  if (action.type === UPDATE_TRAFFIC_VIEW) {
    return {
      ...state,
      view: action.view,
    };
  }

  return state;
};

export default traffic;
