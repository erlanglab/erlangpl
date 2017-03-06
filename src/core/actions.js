// @flow

import * as t from './actionTypes';

export const connectionOpen = () => ({ type: t.CONNECTION_OPEN });

export const connectionClose = () => ({ type: t.CONNECTION_CLOSE });

export const setNode = (node: string) => ({ type: t.SET_NODE, node });
