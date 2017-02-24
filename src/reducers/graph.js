import { UPDATE_GRAPH_DATA, UPDATE_GRAPH_VIEW } from '../actions/graph';

export const INITIAL_STATE = {
  data: {
    nodes: [],
    connections: [],
  },
  view: [],
};

const graph = (state = INITIAL_STATE, action) => {
  if (action.type === UPDATE_GRAPH_DATA) {
    return {
      ...state,
      data: action.data,
    };
  }

  if (action.type === UPDATE_GRAPH_VIEW) {
    return {
      ...state,
      view: action.view,
    };
  }

  return state;
};

export default graph;
