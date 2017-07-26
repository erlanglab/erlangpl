// @flow
// this file is defining public API of epl-vizceral plugin

import Component from './components/Component';
import reducer from './reducer';
import sockets from './sockets';
import * as actions from './actions';

export default { Component, reducer, sockets, actions };
