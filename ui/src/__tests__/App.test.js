import React from 'react';
import { render } from 'react-dom';

import store, { history } from '../store';
import App from '../App';

describe('App component', () => {
  it('should render without crashing', () => {
    render(
      <App store={store} history={history} />,
      document.createElement('div')
    );
  });
});
