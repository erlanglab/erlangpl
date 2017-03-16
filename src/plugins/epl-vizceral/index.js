// @flow
// this file is defining public API of epl-vizceral plugin

import Vizceral from './components/Traffic';
import reducer from './reducer';
import sockets from './sockets';
import * as actions from './actions';

export default { Vizceral, reducer, sockets, actions };
