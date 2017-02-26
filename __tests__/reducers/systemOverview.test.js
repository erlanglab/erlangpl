import reducer, { INITIAL_STATE } from '../../src/reducers/systemOverview';
import { updateSystemInfo } from '../../src/actions/systemInfo';

describe('systemOverview reducer', () => {
  it('should handle initial state', () => {
    expect(reducer(undefined, {})).toEqual(INITIAL_STATE);
  });

  it('should handle UPDATE_SYSTEM_INFO', () => {
    const info = {
      receive: {
        count: 0,
        size: 0,
      },
      memoryTotal: 1123123123,
    };

    expect(reducer(undefined, updateSystemInfo(info))).toEqual({
      receive: [{ count: 0, size: 0 }],
      memoryTotal: [1123123123],
    });
  });

  it('should filter undefined data', () => {
    const info = {
      receive: undefined,
      memoryTotal: undefined,
    };
    const state = reducer(undefined, updateSystemInfo(info));
    expect(state.receive).toHaveLength(0);
    expect(state.memoryTotal).toHaveLength(0);
  });
});
