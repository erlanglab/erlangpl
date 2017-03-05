// @flow
import humps from 'humps';

import { onWithStore } from '../sockets';
import * as actions from './actions';

export default onWithStore((store, on) => {
  return on(
    'epl_dashboard_EPL',
    {
      'system-init': data => {
        const node = humps.camelizeKeys(data).nodeName;
        store.dispatch(actions.setNode(node));
      }
    },
    () => store.dispatch(actions.connectionOpen()),
    () => store.dispatch(actions.connectionClose())
  );
});
