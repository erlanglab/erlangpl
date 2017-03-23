// @flow
import * as actions from './actions';
import { onWithStore } from '../../sockets';

export default onWithStore((store, on) => {
  return on('epl_traffic_EPL', {
    'traffic-info': data => store.dispatch(actions.updateTrafficData(data))
  });
});
