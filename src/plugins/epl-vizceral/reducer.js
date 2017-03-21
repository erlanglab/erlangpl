// @flow
import * as t from './actionTypes';

export const INITIAL_STATE = {
  data: {
    nodes: [],
    connections: []
  },
  search: '',
  view: []
};

const denamify = (name: string) => {
  return name.replace('_at_', '@').replace(/_/g, '.');
};

const denamifyNode = ({ nodes = [], connections = [], ...node }) => {
  return {
    ...node,
    connections: connections.map(({ target, ...connection }) => ({
      ...connection,
      target: denamify(target)
    })),
    nodes: nodes.map(({ name, nodes = [], ...node }) => ({
      ...node,
      name: denamify(name),
      nodes: nodes.map(denamifyNode)
    }))
  };
};

const reducer = (state: any = INITIAL_STATE, action: any) => {
  if (action.type === t.UPDATE_TRAFFIC_DATA) {
    return {
      ...state,
      data: denamifyNode(action.data)
    };
  }

  if (action.type === t.UPDATE_TRAFFIC_VIEW) {
    return {
      ...state,
      search: '',
      view: action.view
    };
  }

  if (action.type === t.UPDATE_TRAFFIC_SEARCH) {
    return {
      ...state,
      search: action.search
    };
  }

  return state;
};

export default reducer;
