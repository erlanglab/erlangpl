// @flow
import * as type from './actionTypes';

const action = type => (...args: any) => ({
  type,
  payload: [...args]
});

export const updateTimelines = action(type.UPDATE_TIMELINES);

export const setCurrentPid = action(type.SET_CURRENT_PID);
export const setCurrentMsg = action(type.SET_CURRENT_MSG);

export const removePid = action(type.REMOVE_PID);
