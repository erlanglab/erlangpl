function WebSocketTest() {
  if ('WebSocket' in window) {
    alert('WebSocket is supported by your Browser!');

    // Let us open a web socket
    var ws = new WebSocket('ws://localhost:37575/epl_ets_EPL');

    ws.onmessage = function(evt) {
      var received_msg = evt.data;
      console.log(received_msg);
      document.getElementById('ets_info').innerHTML = received_msg;
    };

    ws.onclose = function() {
      // websocket is closed.
      alert('Connection is closed...');
    };
  } else {
    // The browser doesn't support WebSocket
    alert('WebSocket NOT supported by your Browser!');
  }
}
window.onload = WebSocketTest;
