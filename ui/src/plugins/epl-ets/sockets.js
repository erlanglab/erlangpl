// @flow
import * as actions from './actions';
import { onWithStore } from '../../sockets';

export default onWithStore((store, on) => {
  return on('epl_ets_EPL', {
    'ets-node-info': data => store.dispatch(actions.updateETSData(data))
  });
});
