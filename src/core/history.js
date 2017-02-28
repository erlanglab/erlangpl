// @flow
import createHistory from 'history/createBrowserHistory';
import { syncHistoryWithStore } from 'react-router-redux';

export const create = store => syncHistoryWithStore(createHistory(), store);
