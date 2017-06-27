// @flow
import * as type from './actionTypes';

export const INITIAL_STATE = {
  timelines: [],
  pid: null,
  msg: 0,
  pidPrefix: ''
};

const reducer = (state: any = INITIAL_STATE, action: any) => {
  if (action.type === type.UPDATE_TIMELINES) {
    const [{ timelines }] = action.payload;
    return {
      ...state,
      timelines: timelines.map(t => ({ ...t, timeline: t.timeline.reverse() }))
    };
  }

  if (action.type === type.SET_CURRENT_PID) {
    const [pid] = action.payload;
    return { ...state, pid, msg: 0 };
  }

  if (action.type === type.SET_CURRENT_MSG) {
    const [msg] = action.payload;
    return { ...state, msg };
  }

  if (action.type === type.SET_INIT) {
    const [{ pid }] = action.payload;
    return { ...state, pidPrefix: pid.replace(/<|>/, '').split('.')[0] };
  }

  if (action.type === 'PUSH_TIMELINE_PID') {
    const pid = action.pid;
    const timeline = { pid, timeline: [] };
    return {
      ...state,
      timelines: [timeline].concat(state.timelines)
    };
  }

  if (action.type === type.REMOVE_PID) {
    const [pid] = action.payload;
    return {
      ...state,
      timelines: state.timelines.filter(t => t.pid !== pid)
    };
  }

  if (action.type === '@@router/LOCATION_CHANGE') {
    const { pathname } = action.payload;
    const pid = pathname.replace(/\/timeline(\/?)/, '');
    return { ...state, pid, msg: 0 };
  }

  return state;
};

export default reducer;
