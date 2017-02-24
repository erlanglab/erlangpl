// @flow

export const UPDATE_GRAPH_DATA = 'UPDATE_GRAPH_DATA';

// TODO (baransu) add proper type adnotation
export const updateGraphData = (data: any) => ({
  type: UPDATE_GRAPH_DATA,
  data,
});

export const UPDATE_GRAPH_VIEW = 'UPDATE_GRAPH_VIEW';

export const updateGraphView = (view: Array<string>) => ({
  type: UPDATE_GRAPH_VIEW,
  view,
});
