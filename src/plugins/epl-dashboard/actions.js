// @flow

import * as t from './actionTypes';

export const updateSystemOverview = (info: mixed) => ({
  type: t.UPDATE_SYSTEM_OVERVIEW,
  info
});

export const updateSystemInfo = (info: mixed) => ({
  type: t.UPDATE_SYSTEM_INFO,
  info
});
