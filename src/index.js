// @flow
import React from 'react';
import { render } from 'react-dom';
import humps from 'humps';
import throttle from 'lodash/throttle';

import App from './App';
import { on, combineSockets, createSockets } from './sockets';

// CSS imports
import 'bootstrap/dist/css/bootstrap.css';
import 'bootstrap/dist/css/bootstrap-theme.css';
import 'font-awesome/css/font-awesome.min.css';
import './index.css';

import core from './core';
import traffic from './traffic';
import home from './home';
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

const handlers = on(
  'epl_dashboard_EPL',
  {
    'system-init': data => {
      const d = humps.camelizeKeys(data);
      store.dispatch(home.actions.updateSystemInfo(humps.camelizeKeys(d)));
    },
    'system-info': data => {
      const d = humps.camelizeKeys(data);
      store.dispatch(home.actions.updateSystemInfo(humps.camelizeKeys(d)));
    }
  },
  () => {
    //onopen
    store.dispatch(core.actions.connectionOpen());
  },
  () => {
    //onclose
    store.dispatch(core.actions.connectionClose());
  }
);

createSockets(
  combineSockets([
    handlers /*, handlers from other plugins or other handlers from the same plugin*/
  ])
);

render(
  <App store={store} history={history} />,
  document.getElementById('root')
);
