import reducer, { INITIAL_STATE } from '../../src/reducers/systemInfo';

describe('systemInfo reducer', () => {
  it('should handle initial state', () => {
    expect(reducer(undefined, {})).toEqual(INITIAL_STATE);
  });
});
