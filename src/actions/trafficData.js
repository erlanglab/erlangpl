import actions from './actions.json';

export const ACTIONS = actions.TRAFFIC_DATA;

export const updateTraffic = data => ({
  type: ACTIONS.UPDATE_TRAFFIC,
  data,
});
