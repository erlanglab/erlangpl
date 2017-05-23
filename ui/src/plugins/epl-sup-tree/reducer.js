// @flow
import { combineReducers } from 'redux';
import * as type from './actionTypes';

const COLORS = {
  supervisor: '#227A50',
  worker: '#1F79B7'
};

const SIZE = {
  supervisor: 5,
  worker: 0.1
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
        label: node.id,
        x: 0, // Math.random(),
        y: 0, // Math.random(),
        size: SIZE[node.type],
        color: COLORS[n.type],
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
    console.log(action.data);
    const d = Object.keys(action.data).reduce(
      ({ nodes, edges }, key) => {
        const node = action.data[key];
        const c = mapNode(node);
        const n = node.type
          ? [
              ...nodes,
              {
                label: node.id,
                x: 0, // Math.random(),
                y: 0, // Math.random(),
                size: SIZE[node.type],
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
    console.log(d);
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
