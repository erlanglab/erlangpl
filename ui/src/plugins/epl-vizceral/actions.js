// @flow

import * as t from './actionTypes';

export const updateTrafficData = (data: any) => ({
  type: t.UPDATE_TRAFFIC_DATA,
  data
});

export const updateTrafficView = (view: Array<string>) => ({
  type: t.UPDATE_TRAFFIC_VIEW,
  view
});

export const updateTrafficSearch = (search: string) => ({
  type: t.UPDATE_TRAFFIC_SEARCH,
  search
});
