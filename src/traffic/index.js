// @flow
// this file is defining public API of traffic module

import Traffic from './components/Traffic';
import reducer from './reducer';
import * as actions from './actions';
import * as actionTypes from './actionTypes';

export default { components: { Traffic }, reducer, actions, actionTypes };
