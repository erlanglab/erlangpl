// @flow
import createHistory from 'history/createBrowserHistory';
import { syncHistoryWithStore } from 'react-router-redux';

import { syncGraphViewWithHistory } from './utils';
import store from './store';
import { updateGraphView } from './actions/graph';

const history = syncHistoryWithStore(createHistory(), store);

history.listen(location => {
  const view = syncGraphViewWithHistory(location);
  store.dispatch(updateGraphView(view));
});

export default history;
