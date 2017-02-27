import React from 'react';
import { render } from 'react-dom';

import store from '../src/store';
import App from '../src/App';

describe('App component', () => {
  it('should render without crashing', () => {
    render(<App store={store} />, document.createElement('div'));
  });
});
