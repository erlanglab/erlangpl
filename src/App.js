// @flow
import React from 'react';
import { BrowserRouter as Router, Route, Redirect } from 'react-router-dom';
import { Provider } from 'react-redux';

import './App.css';

import core from './core';
const { Navigation, Footer } = core.components;

// plugins
import eplDashboard from './plugins/epl-dashboard';
import eplSupTree from './plugins/epl-sup-tree';

/* import traffic from './traffic';*/
import about from './about';

const tabs = [
  { path: '/dashboard', icon: 'television' },
  { path: '/sup-tree', icon: 'sitemap' },
  /*   { path: '/traffic', icon: 'share-alt' },*/
  { path: '/about', icon: 'question' }
];

const App = ({ store, history }: { store: mixed, history: mixed }) => {
  return (
    <Provider store={store}>
      <Router history={history}>
        <div className="App">

          <Navigation tabs={tabs} />

          <div className="App-container">
            <Route exact path="/" render={() => <Redirect to="/dashboard" />} />
            <Route
              path="/dashboard/:subRoute*"
              component={eplDashboard.Dashboard}
            />
            <Route path="/sup-tree" component={eplSupTree.SupTree} />
            {/* <Route
                path="/traffic/:view*"
                component={traffic.components.Traffic}
                /> */
            }
            <Route path="/about" component={about.components.About} />
          </div>
          <Footer />
        </div>
      </Router>
    </Provider>
  );
};

export default App;
