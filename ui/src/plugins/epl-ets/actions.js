// @flow

import * as t from './actionTypes';

export const updateETSData = (data: any) => ({
  type: t.UPDATE_ETS_DATA,
  data
});

export const updateETSView = (view: Array<string>) => ({
  type: t.UPDATE_ETS_VIEW,
  view
});

export const updateETSSearch = (search: string) => ({
  type: t.UPDATE_ETS_SEARCH,
  search
});
