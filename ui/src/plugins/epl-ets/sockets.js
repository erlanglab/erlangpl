// @flow
import * as actions from './actions';
import { onWithStore } from '../../sockets';

import sampleData from './sample-data.json';

export default onWithStore((store, on) => {
  store.dispatch(actions.updateETSData(sampleData.data));
  return on('epl_ets_EPL', {
    'ets-node-info': data => store.dispatch(actions.updateETSData(data))
  });
});
