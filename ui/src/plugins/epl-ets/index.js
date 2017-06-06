// @flow
// this file is defining public API of epl-vizceral plugin

import ETS from './components/ETS';
import reducer from './reducer';
import sockets from './sockets';
import * as actions from './actions';

export default { ETS, reducer, sockets, actions };
