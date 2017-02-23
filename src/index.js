// @flow
import React from 'react';
import { render } from 'react-dom';

import App from './App';

// CSS imports
import 'bootstrap/dist/css/bootstrap.css';
import 'bootstrap/dist/css/bootstrap-theme.css';
import 'font-awesome/css/font-awesome.min.css';
import './index.css';

render(<App />, document.getElementById('root'));
