// @flow
// this file is defining public API of core module

// components
import Navigation from './components/Navigation';
import Footer from './components/Footer';
import Loader from './components/Loader';

import reducer from './reducer';
import * as actions from './actions';
import * as actionTypes from './actionTypes';

import * as history from './history';
import * as utils from './utils';

export default {
  components: { Navigation, Footer, Loader },
  reducer,
  actionTypes,
  actions,
  history,
  utils
};
