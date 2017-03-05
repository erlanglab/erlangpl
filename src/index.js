// @flow
import React from 'react';
import { render } from 'react-dom';
import throttle from 'lodash/throttle';

import App from './App';
import { combineSockets, createSockets } from './sockets';

// CSS imports
import 'bootstrap/dist/css/bootstrap.css';
import 'bootstrap/dist/css/bootstrap-theme.css';
import 'font-awesome/css/font-awesome.min.css';
import './index.css';

// plugins
import eplDashboard from './plugins/epl-dashboard';

import core from './core';
import traffic from './traffic';
import store, { history } from './store';

store.subscribe(
  throttle(
    () => {
      const currentView = store.getState().traffic.view.join('/');
      const { pathname } = window.location;
      const correctView = `/traffic${currentView.length > 0 ? '/' : ''}${currentView}`;
      if (pathname.match(/^\/traffic/) && pathname !== correctView) {
        history.push(correctView);
      }
    },
    100
  )
);

// sync url with store and traffic view (vizceral)
const view = core.utils.syncGraphViewWithHistory(history.location);
store.dispatch(traffic.actions.updateTrafficView(view));

// TODO (baransu) remove dispatching dummy data
import sampleData from './sample_data.json';
store.dispatch(traffic.actions.updateTrafficData(sampleData));

/* register new handlers
   every plugin should return array of handlers which will be passed to
   combineSockets in main application
 */

createSockets(
  combineSockets(
    [
      eplDashboard.sockets,
      core.sockets /*, handlers from other plugins or other handlers from the same plugin*/
    ],
    store
  )
);

render(
  <App store={store} history={history} />,
  document.getElementById('root')
);
