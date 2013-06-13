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
                message('EPL WebSocekt: new');

	        socket.onopen = function(){
	            message('EPL WebSocket: open');
	        }

	        socket.onmessage = function(msg){
                    s = JSON.parse(msg.data);

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
	            message('EPL WebSocket: closed');
	        }

	    } catch(exception){
	        message(exception);
	    }

	    function message(msg){
	        $('#message').text(msg);
            }//End message()

        }//End connect()

    }//End else

    var charts =
        {
            init: function() {
		// init live chart
		this.chart_live.init();
	    },

	    // utility class
	    utility:
	    {
		chartColors: [ "#1F79B7", "#227A50", "#8FBF47", "#6E428F", "#3E4190" ],
		chartBackgroundColors: ["#fff", "#fff"],

		applyStyle: function(that)
		{
		    that.options.colors = charts.utility.chartColors;
		    that.options.grid.backgroundColor = { colors: charts.utility.chartBackgroundColors };
		    that.options.grid.borderColor = charts.utility.chartColors[0];
		    that.options.grid.color = charts.utility.chartColors[0];
		},
	    },

	    // live chart
	    chart_live:
	    {
		// chart data
		data: [],
		totalPoints: 120,
		updateInterval: 5000,

		// we use an inline data source in the example, usually data would
		// be fetched from a server
			getRandomData: function()
			{
				if (this.data.length > 0)
		            this.data = this.data.slice(1);

		        // do a random walk
		        while (this.data.length < this.totalPoints)
			    {
		            var prev = this.data.length > 0 ? this.data[this.data.length - 1] : 50;
		            var y = prev + Math.random() * 10 - 5;
		            if (y < 0)
		                y = 0;
		            if (y > 100)
		                y = 100;
		            this.data.push(y);
		        }

		        // zip the generated y values with the x values
		        var res = [];
		        for (var i = 0; i < this.data.length; ++i)
		            res.push([i, this.data[i]])
		        return res;
			},

			// will hold the chart object
			plot: null,

			// chart options
			options:
			{
				series: {
		        	grow: { active: false },
		        	shadowSize: 0,
		        	lines: {
	            		show: true,
	            		fill: true,
	            		lineWidth: 2,
	            		steps: false
		            }
		        },
		        grid: {
					show: true,
				    aboveData: false,
				    color: "#3f3f3f",
				    labelMargin: 5,
				    axisMargin: 0,
				    borderWidth: 0,
				    borderColor:null,
				    minBorderMargin: 5 ,
				    clickable: true,
				    hoverable: true,
				    autoHighlight: false,
				    mouseActiveRadius: 20,
				    backgroundColor : { }
				},
				colors: [],
		        tooltip: true,
				tooltipOpts: {
					content: "Value is : %y.0",
					shifts: {
						x: -30,
						y: -50
					},
					defaultTheme: false
				},
		        yaxis: { min: 0, max: 100 },
		        xaxis: { show: true}
			},

			// initialize
			init: function()
			{
				// apply styling
				charts.utility.applyStyle(this);

				this.plot = $.plot($("#chart_live"), [ this.getRandomData() ], this.options);
				setTimeout(this.update, charts.chart_live.updateInterval);
			},

			// update
			update: function()
			{
				charts.chart_live.plot.setData([ charts.chart_live.getRandomData() ]);
		        charts.chart_live.plot.draw();

		        setTimeout(charts.chart_live.update, charts.chart_live.updateInterval);
			}
		}
			};

	$(function()
	{
		// initialize charts
		charts.init();
	});

    connect();

});
