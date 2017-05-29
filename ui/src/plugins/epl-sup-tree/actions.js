// @flow
import * as type from './actionTypes';

export const updateAppsInfo = (data: any) => ({
  type: type.UPDATE_APPS_INFO,
  data
});

export const updateNodeInfo = (info: any) => ({
  type: type.UPDATE_NODE_INFO,
  info
});

export const selectApps = (apps: Array<string>) => ({
  type: type.SELECT_APPS,
  apps
});

export const clearApps = (apps: Array<string>) => ({
  type: type.CLEAR_APPS,
  apps
});

export const center = (x: number, y: number) => ({
  type: type.CENTER,
  x,
  y
});
