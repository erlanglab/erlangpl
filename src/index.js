// @flow
import React from 'react';
import { render } from 'react-dom';
console.log(new Image('/epl-plugin/test.png'));
// TODO handle this in separate file, excluded in production build

// loading plugins
const plugins = Object.keys(window)
  .filter(key => {
    return key.match(/^__EPL_/);
  })
  .map(key => console.log('Loaded:', key.replace('__EPL_', '')) || window[key]);
console.log(plugins);
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

import core from './core';
import store, { history } from './store';

/* register new handlers
   every plugin should return array of handlers which will be passed to
   combineSockets in main application
 */
createSockets(
  combineSockets(
    [
      eplDashboard.sockets,
      eplSupTree.sockets,
      eplVizceral.sockets,
      core.sockets /*, handlers from other plugins or other handlers from the same plugin*/
    ],
    store
  )
);

render(
  <App store={store} history={history} />,
  document.getElementById('root')
);
