// @flow
import store from './store';
import { connectionOpen, connectionClose } from './actions/connection';

// right now looking for erlangpl runing standalone at localhost:8000
const { hostname } = window.location;
const socket = new WebSocket(
  `ws://${hostname}:8000/epl_dashboard_EPL` /* protocols */
);

let defaultHandler = {};
// registers new handler for given topic, it will be used in onmessage
export const on = (
  topic: string,
  callback: () => void,
  handler: any = defaultHandler
) => {
  handler[topic] = callback;
};

socket.onopen = e => {
  store.dispatch(connectionOpen());
};

// when messages come, it will find handler and run them with msg data
socket.onmessage = (msg: any) => {
  const { topic, data } = JSON.parse(msg.data);
  if (typeof defaultHandler[topic] === 'function') {
    defaultHandler[topic](data);
  } else {
    console.warn('Could not find handler for', topic);
  }
};

socket.onclose = () => {
  store.dispatch(connectionClose());
};

// socket.send(data: string)
// socket.close()
//export default socket;
