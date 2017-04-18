// @flow
import * as type from './actionTypes';
import { Map } from 'immutable';

export const INITIAL_STATE = Map({
  timelines: [],
  pid: null,
  msg: 0
});

const reducer = (state: any = INITIAL_STATE, action: any) => {
  if (action.type === type.UPDATE_TIMELINES) {
    const [{ timelines }] = action.payload;
    console.log(timelines);
    return state.set(
      'timelines',
      timelines.map(t => ({ ...t, timeline: t.timeline.reverse() }))
    );
  }

  if (action.type === type.SET_CURRENT_PID) {
    const [pid] = action.payload;
    return state.set('pid', pid);
  }

  if (action.type === type.SET_CURRENT_MSG) {
    const [msg] = action.payload;
    return state.set('msg', msg);
  }

  if (action.type === 'PUSH_TIMELINE_PID') {
    const pid = action.pid;
    return state.update('timelines', timelines =>
      [
        {
          pid,
          timeline: []
        }
      ].concat(timelines)
    );
  }

  return state;
};

export default reducer;
