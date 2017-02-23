// @flow
import React from 'react';
import Router from 'react-router-dom/BrowserRouter';
import Route from 'react-router/Route';
import { Provider } from 'react-redux';
import { syncHistoryWithStore } from 'react-router-redux';
import createHistory from 'history/createBrowserHistory';

import Navigation from './components/Navigation';
import Home from './components/Home';
import Page from './components/Page';
import About from './components/About';
import store from './store';

const history = syncHistoryWithStore(createHistory(), store);

const App = () => {
  return (
    <Provider store={store}>
      <Router history={history}>
        <div>

          {/* TODO (baransu) maybe pass routes as param? */}
          <Navigation />

          <Route exact path="/" component={Home} />
          <Route path="/page" component={Page} />
          <Route path="/about" component={About} />
        </div>
      </Router>
    </Provider>
  );
};

export default App;
