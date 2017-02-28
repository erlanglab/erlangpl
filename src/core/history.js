// @flow
import createHistory from 'history/createBrowserHistory';
import { syncHistoryWithStore } from 'react-router-redux';

export const create = (store: mixed) =>
  syncHistoryWithStore(createHistory(), store);
