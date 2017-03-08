// @flow
import { combineReducers } from 'redux';
import * as t from './actionTypes';

const treeReducer = (state: any = {}, action: any) => {
  if (action.type === t.UPDATE_APPS_INFO) {
    return action.data;
  }
  return state;
};

const nodeInfoReducer = (state: any = null, action: any) => {
  if (action.type === t.UPDATE_NODE_INFO) {
    return action.info;
  }
  return state;
};

export default combineReducers({
  tree: treeReducer,
  nodeInfo: nodeInfoReducer
});
