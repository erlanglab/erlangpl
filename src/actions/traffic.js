// @flow

export const UPDATE_TRAFFIC_DATA = 'UPDATE_TRAFFIC_DATA';

// TODO (baransu) add proper type adnotation
export const updateTrafficData = (data: any) => ({
  type: UPDATE_TRAFFIC_DATA,
  data,
});

export const UPDATE_TRAFFIC_VIEW = 'UPDATE_TRAFFIC_VIEW';

export const updateTrafficView = (view: Array<string>) => ({
  type: UPDATE_TRAFFIC_VIEW,
  view,
});
