// @flow
// this file is defining public API of home module

import Home from './components/Home';
import reducer from './reducer';

import * as actions from './actions';
import * as actionTypes from './actionTypes';

export default { components: { Home }, reducer, actions, actionTypes };
