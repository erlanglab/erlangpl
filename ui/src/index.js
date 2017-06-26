// @flow
import React from 'react';
import { render } from 'react-dom';

import App from './App';
import { combineSockets, createSockets } from './sockets';

// CSS imports
import 'bootstrap/dist/css/bootstrap.css';
import 'bootstrap/dist/css/bootstrap-theme.css';
import 'font-awesome/css/font-awesome.min.css';
import './index.css';

// plugins
import eplDashboard from './plugins/epl-dashboard';
import eplSupTree from './plugins/epl-sup-tree';
import eplVizceral from './plugins/epl-vizceral';
import eplTimeline from './plugins/epl-timeline';
import eplETS from './plugins/epl-ets';

import core from './core';
import store, { history } from './store';

/* register new handlers
   every plugin should return handlers which will be passed to
   combineSockets in main application
 */
createSockets(
  combineSockets(
    [
      eplDashboard.sockets,
      eplSupTree.sockets,
      eplVizceral.sockets,
      eplTimeline.sockets,
      eplETS.sockets,
      ...core.sockets
      /* other handlers */
    ],
    store
  )
);

render(
  <App store={store} history={history} />,
  document.getElementById('root')
);
