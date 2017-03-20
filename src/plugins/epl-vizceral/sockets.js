// @flow
import { onWithStore } from '../../sockets';
export default onWithStore((store, on) => {
  return on(
    'epl_dashboard_EPL',
    {
      'traffic-info': data => {
        console.log(data);
        // dispatch some action to store this
        /* console.log('apps-info', data);
         * store.dispatch(actions.updateAppsInfo(data));*/
      }
    },
    () => {
      console.log('epl-traffic open');
    },
    () => {
      console.log('epl-traffic close');
    }
  );
});
