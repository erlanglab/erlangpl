import { syncGraphViewWithHistory as historyToView } from '../src/utils';
import createHistory from 'history/createBrowserHistory';

describe('syncGraphViewWithHistory', () => {
  let history;

  beforeEach(() => {
    history = createHistory();
  });

  it('should handle undefined', () => {
    expect(historyToView(undefined)).toEqual([]);
  });

  it('should handle graph path', () => {
    history.push('/graph/us-west-1');
    expect(historyToView(history.location)).toEqual(['us-west-1']);
  });
  it('should handle other path', () => {
    history.push('/home');
    expect(historyToView(history.location)).toEqual([]);
  });
});
