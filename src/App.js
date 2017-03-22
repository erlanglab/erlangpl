// @flow
import React from 'react';
import { Route, Redirect } from 'react-router-dom';
import { Provider } from 'react-redux';
import { ConnectedRouter } from 'react-router-redux';

import './App.css';

import core from './core';
const { Navigation, Footer } = core.components;

// plugins
import eplDashboard from './plugins/epl-dashboard';
import eplSupTree from './plugins/epl-sup-tree';
import eplVizceral from './plugins/epl-vizceral';

import about from './about';

const tabs = [
  { path: '/dashboard', icon: 'television' },
  { path: '/sup-tree', icon: 'sitemap' },
  { path: '/traffic', icon: 'share-alt' },
  { path: '/about', icon: 'question' }
];

const App = ({ store, history }: { store: mixed, history: mixed }) => {
  return (
    <Provider store={store}>
      <ConnectedRouter history={history}>
        <div className="App">

          <Navigation tabs={tabs} />

          <div className="App-container">
            <Route exact path="/" render={() => <Redirect to="/dashboard" />} />
            <Route
              path="/dashboard/:subRoute*"
              component={eplDashboard.Dashboard}
            />
            <Route path="/sup-tree" component={eplSupTree.SupTree} />
            <Route path="/traffic/:view*" component={eplVizceral.Vizceral} />
            <Route path="/about" component={about.components.About} />
          </div>
          <Footer />
        </div>
      </ConnectedRouter>
    </Provider>
  );
};

export default App;
