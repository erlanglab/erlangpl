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
import eplTimeline from './plugins/epl-timeline';

import about from './about';

import plugins from './plugins';

const App = ({ store, history }: { store: mixed, history: mixed }) => {
  const tabs = [
    { path: '/dashboard', icon: 'television' },
    { path: '/sup-tree', icon: 'sitemap' },
    { path: '/traffic', icon: 'share-alt' },
    { path: '/timeline', icon: 'repeat' },
    { path: '/about', icon: 'question' }
  ].concat(
    plugins.reduce((acc, plugin) => {
      if (plugin.name && plugin.icon) {
        const name = plugin.name.replace('epl-', '');
        return acc.concat({ path: `/${name}`, icon: plugin.icon });
      }
      console.warn(`Could not register navigation for ${plugin.name}`);
      return acc;
    }, [])
  );

  const routes = plugins.reduce((acc, plugin) => {
    if (plugin.name && plugin.Component) {
      const name = plugin.name.replace('epl-', '');
      return acc.concat(
        <Route
          key={acc.length}
          path={`/${name}`}
          component={plugin.Component}
        />
      );
    }
    console.warn(`Could not add route for ${plugin.name}`);
    return acc;
  }, []);

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
            <Route path="/timeline" component={eplTimeline.Component} />
            <Route path="/about" component={about.components.About} />
            {routes}
          </div>
          <Footer />
        </div>
      </ConnectedRouter>
    </Provider>
  );
};

export default App;
