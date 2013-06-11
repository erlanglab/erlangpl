$(document).ready(function() {
    var socket;

    if(!("WebSocket" in window)) {
        $('<p>Oh no, you need a browser that supports WebSockets.'+
          'How about <a href="http://www.getfirefox.com/">Mozilla Firefox</a>?'+
          '</p>').appendTo('#container');
    } else {
        //
        //The user has WebSockets
        //
        function connect(){
            var host = "ws://"+window.location.hostname+":"+
                window.location.port+"/epl_dashboard_EPL";
            try{
                socket = new WebSocket(host);
                message('WebSocekt: new');

	        socket.onopen = function(){
	            message('WebSocket: open');
	        }

	        socket.onmessage = function(msg){
                    s = JSON.parse(msg.data);

                    console.log(s);

                    // Node name
                    if(s.node_name != undefined) { $('#node_name').text(s.node_name)};

		    // Node counters
                    if(s.spawn != undefined) { $('#spawn_count').text(s.spawn.count)};
                    if(s.exit != undefined) {
                        $('#exit_count').text(s.exit.count);
                        $('#exit_abnormal').text(s.exit.abnormal);
                    };
                    if(s.receive != undefined) {
                        $('#receive_count').text(s.receive.count + ' msg');
                        $('#receive_sizes').text(s.receive.sizes + ' B');
                    };
                    if(s.process_count != undefined) { $('#process_count').text(s.process_count)};
                    if(s.memory_total != undefined) { $('#memory_total').text(s.memory_total + ' B')};

		    // Node settings
                    if(s.garbage_collection != undefined) { $('#fullsweep_after').text(s.garbage_collection.fullsweep_after)};
                    if(s.node_pid           != undefined) { $('#node_pid').text(s.node_pid)};
                    if(s.build_type         != undefined) { $('#build_type').text(s.build_type)};
                    if(s.compat_rel         != undefined) { $('#compat_rel').text(s.compat_rel)};
                    if(s.port_limit         != undefined) { $('#port_limit').text(s.port_limit)};
                    if(s.kernel_poll        != undefined) { $('#kernel_poll').text(s.kernel_poll)};
                    if(s.otp_release        != undefined) { $('#otp_release').text(s.otp_release)};
                    if(s.process_limit      != undefined) { $('#process_limit').text(s.process_limit)};
                    if(s.schedulers         != undefined) { $('#schedulers').text(s.schedulers)};
                    if(s.smp_support        != undefined) { $('#smp_support').text(s.smp_support)};
                    if(s.threads            != undefined) { $('#threads').text(s.threads)};
                    if(s.thread_pool_size   != undefined) { $('#thread_pool_size').text(s.thread_pool_size)};
                    if(s.version            != undefined) { $('#version').text(s.version)};
                    if(s.wordsize           != undefined) { $('#wordsize').text(s.wordsize)};
                }

	        socket.onclose = function(){
	            message('WebSocket: closed');
	        }

	    } catch(exception){
	        message(exception);
	    }

	    function message(msg){
	        $('#message').text(msg);
            }//End message()

        }//End connect()

    }//End else

    connect();

});
