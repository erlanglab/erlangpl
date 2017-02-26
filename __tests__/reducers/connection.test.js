import reducer, { INITIAL_STATE } from '../../src/reducers/connection';
import { connectionOpen, connectionClose } from '../../src/actions/connection';

describe('connection reducer', () => {
  it('should handle initial state', () => {
    expect(reducer(undefined, {})).toBe(INITIAL_STATE);
  });

  it('should handle new connection', () => {
    expect(reducer(undefined, connectionOpen())).toBe('connected');
  });
  it('should handle connection list', () => {
    expect(reducer(undefined, connectionClose())).toBe('disconnected');
  });
});
