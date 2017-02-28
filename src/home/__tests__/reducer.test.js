import reducer, {
  INITIAL_STATE_SYSTEM_INFO,
  INITIAL_STATE_SYSTEM_OVERVIEW
} from '../reducer';
import * as actions from '../actions';

describe('systemOverview reducer', () => {
  it('should handle initial state', () => {
    expect(reducer(undefined, {}).systemOverview).toEqual(
      INITIAL_STATE_SYSTEM_OVERVIEW
    );
  });

  it('should handle UPDATE_SYSTEM_INFO', () => {
    const info = {
      receive: {
        count: 0,
        size: 0
      },
      memoryTotal: 1123123123
    };

    expect(
      reducer(undefined, actions.updateSystemInfo(info)).systemOverview
    ).toEqual({
      receive: [{ count: 0, size: 0 }],
      memoryTotal: [1123123123]
    });
  });

  it('should filter undefined data', () => {
    const info = {
      receive: undefined,
      memoryTotal: undefined
    };
    const state = reducer(
      undefined,
      actions.updateSystemInfo(info)
    ).systemOverview;
    expect(state.receive).toHaveLength(0);
    expect(state.memoryTotal).toHaveLength(0);
  });
});

describe('systemInfo reducer', () => {
  it('should handle initial state', () => {
    expect(reducer(undefined, {}).systemInfo).toEqual(
      INITIAL_STATE_SYSTEM_INFO
    );
  });

  // TODO (baransu) check if action updates store
});
