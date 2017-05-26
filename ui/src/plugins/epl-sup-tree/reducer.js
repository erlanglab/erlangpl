// @flow
import { combineReducers } from 'redux';
import * as type from './actionTypes';
import { COLORS } from './constants';

const SIZE = {
  supervisor: 5,
  worker: 3
};

const INITIAL_STATE = { nodes: [], edges: [] };

function mapNode(node) {
  const { nodes, edges } = (node.children || []).reduce((
    { nodes, edges },
    node
  ) => {
    const c = mapNode(node);
    return {
      nodes: nodes.concat(c.nodes),
      edges: edges.concat(c.edges)
    };
  }, { nodes: [], edges: [] });

  return {
    nodes: (node.children || [])
      .map(n => ({
        label: n.id,
        x: 0, // Math.random(),
        y: 0, // Math.random(),
        size: SIZE[n.type],
        color: COLORS[n.type],
        type: n.type,
        id: n.id,
        shape: 'square'
      }))
      .concat(nodes),
    edges: (node.children || [])
      .map(c => ({
        id: `f${node.id}t${c.id}`,
        color: '#9da5b4', // '#63666A', // '#21252b',
        size: 0.1,
        source: node.id,
        target: c.id
      }))
      .concat(edges)
  };
}

function treeReducer(state: any = INITIAL_STATE, action: any) {
  if (action.type === type.UPDATE_APPS_INFO) {
    const d = Object.keys(action.data).reduce(
      ({ nodes, edges }, key) => {
        const node = action.data[key];
        const c = mapNode(node);
        const n = node.type
          ? [
              ...nodes,
              {
                label: key, // node.id,
                x: 0, // Math.random(),
                y: 0, // Math.random(),
                size: SIZE[node.type],
                type: node.type,
                color: COLORS[node.type],
                id: node.id
              }
            ].concat(c.nodes)
          : nodes;

        const e = edges.concat(c.edges);

        return { nodes: n, edges: e };
      },
      { nodes: [], edges: [] }
    );
    return d;
  }
  return state;
}

function nodeInfoReducer(state: any = null, action: any) {
  if (action.type === type.UPDATE_NODE_INFO) {
    return action.info;
  }
  return state;
}

export default combineReducers({
  tree: treeReducer,
  nodeInfo: nodeInfoReducer
});
