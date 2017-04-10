import reducer, { INITIAL_STATE } from '../reducer';
import * as actions from '../actions';

describe('connection reducer', () => {
  it('should handle initial state', () => {
    expect(reducer(undefined, {}).connection).toBe(INITIAL_STATE);
  });

  it('should handle new connection', () => {
    expect(reducer(undefined, actions.connectionOpen()).connection).toBe(
      'connected'
    );
  });
  it('should handle connection list', () => {
    expect(reducer(undefined, actions.connectionClose()).connection).toBe(
      'disconnected'
    );
  });
});
