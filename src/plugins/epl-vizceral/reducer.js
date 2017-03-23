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
    const data = denamifyNode(action.data);
    /*     console.log('data', JSON.stringify(data));*/
    return {
      ...state,
      data
    };
  }

  if (action.type === '@@router/LOCATION_CHANGE') {
    const { pathname } = action.payload;
    const view = pathname
      .replace(/\/traffic(\/?)/, '')
      .split('/')
      .filter(a => a.length > 0);
    if (view.toString() !== state.view.toString()) {
      return {
        ...state,
        view
      };
    }
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
