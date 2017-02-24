import React from 'react';
import { render } from 'react-dom';
import App from '../src/App.js';

describe('App component', () => {
  it('renders without crashing', () => {
    const div = document.createElement('div');
    render(<App />, div);
  });
});
