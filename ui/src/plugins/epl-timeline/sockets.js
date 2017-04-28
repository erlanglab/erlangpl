// @flow
import * as actions from './actions';
import { onWithStore } from '../../sockets';

export default onWithStore((store, on) => {
  return on('epl_timeline_EPL', {
    'timeline-init': data => store.dispatch(actions.setInit(data)),
    'timeline-info': data => store.dispatch(actions.updateTimelines(data))
  });
});
