// @flow
import humps from 'humps';

import { onWithStore } from '../../sockets';
import * as actions from './actions';

export default onWithStore((store, on) => on('epl_dashboard_EPL', {
  'system-init': data => {
    const d = humps.camelizeKeys(data);
    store.dispatch(actions.updateSystemInfo(humps.camelizeKeys(d)));
  },
  'system-info': data => {
    const d = humps.camelizeKeys(data);
    store.dispatch(actions.updateSystemInfo(humps.camelizeKeys(d)));
  }
}));
