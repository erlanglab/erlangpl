// @flow
import humps from 'humps';

import { onWithStore } from '../sockets';
import * as actions from './actions';

export default [
  (store, on) =>
    on(
      'epl_dashboard_EPL',
      {
        'system-init': data => {
          const { nodeName, startedAt } = humps.camelizeKeys(data);
          store.dispatch(actions.setNode(nodeName));
          store.dispatch(actions.setTimestamp(startedAt));
        }
      },
      () => store.dispatch(actions.connectionOpen()),
      () => store.dispatch(actions.connectionClose())
    )
].map(onWithStore);
