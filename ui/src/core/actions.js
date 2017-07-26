// @flow

import * as type from './actionTypes';

export const connectionOpen = () => ({ type: type.CONNECTION_OPEN });

export const connectionClose = () => ({ type: type.CONNECTION_CLOSE });

export const setNode = (node: string) => ({ type: type.SET_NODE, node });

export const setTimestamp = (timestamp: number) => ({
  type: type.SET_TIMESTAMP,
  timestamp
});
