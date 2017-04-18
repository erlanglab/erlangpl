import reducer, { INITIAL_STATE } from '../reducer';

describe('timeline reducer', () => {
  it('should handle initial state', () => {
    expect(reducer(undefined, {})).toEqual(INITIAL_STATE);
  });
});
