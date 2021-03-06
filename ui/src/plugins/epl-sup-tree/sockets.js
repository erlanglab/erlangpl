// @flow
import { onWithStore } from '../../sockets';
import * as actions from './actions';

export default onWithStore((store, on) => {
  return on('epl_st_EPL', {
    'apps-info': data => {
      // dispatch some action to store this
      /*         console.log('apps-info', data);*/
      store.dispatch(actions.updateAppsInfo(data));
    },
    'node-info': data => {
      /*         console.log('node-info', data);*/
      store.dispatch(actions.updateNodeInfo(data));
    }
  });
});
