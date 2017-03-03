$(document).ready(function() {
  var socket;
  var applications = {}

  var sup_node = {size: 3,
                  color: 0x77bbFF,
                  opacity: 0.5};
  var worker_node = {size:2,
                     color: 0x777777,
                     opacity: 0.7};
  var default_edge = { sizeS: 0.9,
                       sizeT: 0.9,
                       color: 0x888888,
                       opacity: 1 };

  $('#app-select').change(function() {
    $("#app-select option:selected").each(function() {
      app = $(this).text();
      console.log("Drawing tree for " + app);
      drawTree(app);
    });
  });

  if(!("WebSocket" in window)) {
    $('<p>Oh no, you need a browser that supports WebSockets.'+
      'How about <a href="http://www.getfirefox.com/">Mozilla Firefox</a>?'+
      '</p>').appendTo('#epl_st');
  } else {
    //
    //The user has WebSockets
    //
    function connect(){
      var host = "ws://"+window.location.hostname+":"+
                 window.location.port+"/epl_st_EPL";
      try{
        socket = new WebSocket(host);
        message('3D WebSocket: new');

        socket.onopen = function(){
          message('3D WebSocket: open');
        }

        socket.onmessage = function(msg){
          d = JSON.parse(msg.data).data;
          console.log(d);

          if(d.info != undefined) {
            $('#process_info').html(JSON.stringify(d.info, undefined, 2));
          };

          if(d.applications != undefined) {
            for (var app in d.applications) {
              if (applications[app] == undefined) {
                applications[app] = d.applications[app];
                $('#app-select').append(
                  $('<option></option>')
                    .attr('value', app)
                    .text(app));
              }
            };
          };
        }

        socket.onclose = function(){
          message('3D WebSocket: closed');
        }

      } catch(exception){
        message(exception);
      }

      function message(msg){
        $('#message').text(msg);
      }//End message()

    }//End connect()

  }//End else

  function drawTree(appName) {
    topSup = applications[appName];
    VE.setNode(topSup.id, sup_node);
    drawChildren(topSup);
  }

  function drawChildren(node) {
    if (node.children.length == 0) return;
    else {
      node.children.forEach(function(child) {
        drawNode(child);
        VE.setEdge(node.id, child.id, default_edge);
        drawChildren(child);
      })
    }
  }

  function drawNode(node) {
    if(node.type == "worker") VE.setNode(node.id, worker_node);
    else VE.setNode(node.id, sup_node);
  }

  connect();

  VE.initialize('epl_st', { transparency: true });
  VE.onNodeSelect = function onSelect(nodeId) {
    VE.focusNode(nodeId);
    socket.send(nodeId);
  }

});
