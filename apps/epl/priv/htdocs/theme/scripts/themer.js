function themerUpdateColors(primary,secondary)
{
	updatePrimaryColor(primary);
	updateHeaderColor(secondary, true, true);
}

//Converts an RGB object to a hex string
function rgb2hex(rgb)
{
	var hex = [
		rgb.r.toString(16),
		rgb.g.toString(16),
		rgb.b.toString(16)
	];
	$.each(hex, function(nr, val) {
		if (val.length === 1) hex[nr] = '0' + val;
	});
	return '#' + hex.join('');
}

// converts a string to RGB object
function rgbString2obj(string)
{
	var parts = string.match(/^rgb\((\d+),\s*(\d+),\s*(\d+)\)$/);
	var rgbObj = { r: Number(parts[1]), g: Number(parts[2]), b: Number(parts[3]) };
	return rgbObj;
}

function updatePrimaryColor(hex, attach, charts)
{
	themerPrimaryColor = hex;
	$('#themer-primary-cp').val(themerPrimaryColor);
	$.minicolors.refresh();

	if (attach === true)
		attachStylesheet();

	if (charts === true)
		updateCharts();

	if (themerPrimaryColor != themerThemes[themerSelectedTheme].primaryColor)
		themerCustom[themerSelectedTheme].primaryColor = themerPrimaryColor;
	else
		themerCustom[themerSelectedTheme].primaryColor = null;

	$.cookie('themerCustom', JSON.stringify(themerCustom));

	toggleGetCode();
}

function toggleGetCode()
{
	var tcs = themerCustom[themerSelectedTheme];

	if (themerSelectedTheme != 0 || (themerSelectedTheme == 0 && (tcs.primaryColor != null || tcs.headerColor != null)))
	{
		if ($('#themer-getcode').is(':hidden')) $('#themer-getcode').show();
	}
	else
	{
		if ($('#themer-getcode').is(':visible')) $('#themer-getcode').hide();
	}
}

function updateHeaderColor(hex, attach, charts)
{
	themerHeaderColor = hex;
	$('#themer-header-cp').val(themerHeaderColor);
	$.minicolors.refresh();

	if (attach === true)
		attachStylesheet();

	if (charts === true)
		updateCharts();

	if (themerHeaderColor != themerThemes[themerSelectedTheme].headerColor)
		themerCustom[themerSelectedTheme].headerColor = themerHeaderColor;
	else
		themerCustom[themerSelectedTheme].headerColor = null;

	$.cookie('themerCustom', JSON.stringify(themerCustom));

	toggleGetCode();
}

function updateMenuColor(hex, attach, charts)
{
	themerMenuColor = hex;
	$('#themer-menu-cp').val(themerMenuColor);
	$.minicolors.refresh();

	if (attach === true)
		attachStylesheet();

	if (charts === true)
		updateCharts();

	if (themerMenuColor != themerThemes[themerSelectedTheme].menuColor)
		themerCustom[themerSelectedTheme].menuColor = themerMenuColor;
	else
		themerCustom[themerSelectedTheme].menuColor = null;

	$.cookie('themerCustom', JSON.stringify(themerCustom));

	toggleGetCode();
}

var themerAdvanced = $.cookie('themerAdvanced') != null ? $.cookie('themerAdvanced') == true : false;
function themerAdvancedToggle()
{
	var cp = [$('#themer-primary-cp'), $('#themer-header-cp'), $('#themer-menu-cp')];

	if ($('#themer-advanced-toggle').is(':checked'))
	{
		$('#themer').addClass('themer-advanced');
		$.each(cp, function(k,v){ v.attr('data-textfield', true).removeClass('minicolors-hidden'); });
	}
	else
	{
		$('#themer').removeClass('themer-advanced');
		$.each(cp, function(k,v){ v.attr('data-textfield', false).addClass('minicolors-hidden'); });
	}
}

function generateCSS(basePath)
{
	if(!basePath)
		basePath = "";

	var css =
		"@primaryColor: " + themerPrimaryColor + ";\n" +
		"@headerColor: " + themerHeaderColor + ";\n" +
		"@headerBorderColor: contrast(@headerColor, lighten(@headerColor, 2%), darken(@headerColor, 10%));\n" +
		"@headerBorderColor2: contrast(@headerBorderColor, darken(@headerBorderColor, 30%), lighten(@headerBorderColor, 30%));\n" +
		"@headerTextColor: contrast(@headerColor, darken(@headerColor, 50%), lighten(@headerColor, 50%));\n" +
		"@headerTextShadowColor: contrast(@headerTextColor, darken(@headerColor, 20%), lighten(@headerColor, 20%));\n\n" +
		"@headerNotifColor: contrast(@headerColor, @primaryColor, lighten(@primaryColor, 20%));\n\n" +

		"@menuColor: " + themerMenuColor + ";\n" +
		"@menuBorderColor: contrast(@menuColor, lighten(@menuColor, 2%), darken(@menuColor, 10%));\n" +
		"@menuBorderColor2: contrast(@menuBorderColor, darken(@menuBorderColor, 30%), lighten(@menuBorderColor, 30%));\n" +
		"@menuTextColor: contrast(@menuColor, darken(@menuColor, 50%), lighten(@menuColor, 50%));\n" +
		"@menuTextShadowColor: contrast(@menuTextColor, darken(@menuColor, 20%), lighten(@menuColor, 20%));\n\n" +

		primaryBgColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	background-color: @primaryColor;\n"+
		"}\n\n" +
		primaryTextColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	color: @primaryColor;\n"+
		"}\n\n" +
		".navbar.main .profile em {\n" +
		"	color: contrast(@headerColor, darken(@primaryColor, 10%), lighten(@primaryColor, 40%));\n"+
		"}\n\n" +
		".navbar.main .notif li a i:before {\n" +
		"	color: contrast(@headerNotifColor, darken(@primaryColor, 5%), lighten(@primaryColor, 20%));\n"+
		"}\n\n" +
		".navbar.main .notif li a {\n" +
		"	background-color: @headerNotifColor;\n"+
		"}\n\n" +
		primaryBorderColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	border-color: @primaryColor;\n"+
		"}\n\n";

	if (themerCustom[themerSelectedTheme].headerColor != null || themerSelectedTheme != '0')
	{
	css += headerBgColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	background-color:" + themerHeaderColor + ";\n"+
		"}\n\n" +
		headerBorderColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	border-color: @headerBorderColor;\n" +
		"}\n\n" +
		headerBorderColorMixedTargets.join(", \n") + "\n" +
		"{\n" +
		"	border-bottom-color: @headerBorderColor;\n" +
		"	border-top-color: @headerBorderColor2;\n" +
		"	border-left-color: @headerBorderColor;\n" +
		"	border-right-color: @headerBorderColor2;\n" +
		"}\n\n" +
		headerTextColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	color: @headerTextColor;\n" +
		"	text-shadow: 0 1px 0 @headerTextShadowColor;\n" +
		"}\n\n" +
		headerBoxShadowColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	box-shadow: 0 0 0 1px @headerBorderColor2;" + "\n" +
		"	-webkit-box-shadow: 0 0 0 1px @headerBorderColor2;" + "\n" +
		"	-moz-box-shadow: 0 0 0 1px @headerBorderColor2;" +
		"}\n\n" +
		headerCaretColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	border-top-color: @headerTextColor;\n" +
		"	border-bottom-color: @headerTextColor;\n" +
		"}\n\n" +
		".navbar.main .notif li a {\n" +
		"	background-color: contrast(@headerColor, @primaryColor, lighten(@primaryColor, 20%));\n"+
		"}\n\n";
	}

	if (themerCustom[themerSelectedTheme].menuColor != null || themerSelectedTheme != '0')
	{
	css += sidebarBgColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	background-color: @menuColor;\n"+
		"}\n\n" +
		sidebarBgColor2Targets.join(", \n") + "\n" +
		"{\n" +
		"	background-color: darken(@menuColor, 15%);\n"+
		"}\n\n" +
		sidebarBorderColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	border-color: @menuBorderColor;\n"+
		"}\n\n" +
		sidebarBorderColor2Targets.join(", \n") + "\n" +
		"{\n" +
		"	border-color: @menuBorderColor2;\n"+
		"}\n\n" +
		sidebarTextColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	color: @menuTextColor;\n" +
		"	text-shadow: 0 1px 0 @menuTextShadowColor;\n" +
		"}\n\n" +
		".col.main-left ul li.active i:before {\n" +
		"	text-shadow: none;\n" +
		"}\n\n" +
		contentBgColorTargets.join(", \n") + "\n" +
		"{\n" +
		"	background-color: #ffffff;\n"+
		"}\n\n";
	}

	css +=
		".table-primary tbody td\n" +
		"{\n" +
		"	background-color: lighten(@primaryColor, 50%);\n" +
		"}\n\n" +
		".table-primary tbody tr.selected td, .table-primary tbody tr.selectable:hover td\n" +
		"{\n" +
		"	background-color: lighten(@primaryColor, 40%);\n" +
		"}\n\n" +
		".table-primary.table-bordered tbody td, .table-primary, .pagination ul > .disabled > a, .pagination ul > .disabled > span\n" +
		"{\n" +
		"	border-color: lighten(@primaryColor, 50%);\n" +
		"}\n\n" +
		".widget .widget-body.list.list-2 ul li\n" +
		"{\n" +
		"	&.active { border-color: lighten(@primaryColor, 20%); }\n" +
		"	a { color: lighten(@primaryColor, 20%); i:before { background: lighten(@primaryColor, 50%); color: lighten(@primaryColor, 10%); border-color: lighten(@primaryColor, 20%); } }\n" +
		"}";

	return css;
}

function attachStylesheet(basePath, reset)
{
	/*if(!$("#themer-stylesheet").length) $('body').append('<div id="themer-stylesheet"></div>');
	$("#themer-stylesheet").html($('<style type="text/less">' + generateCSS(basePath) + '</style>'));*/

	if (themerSelectedTheme == 0)
	{
		$('#themer-stylesheet').empty();
		less.refreshStyles();
		if (reset === true) return false;
	}

	if(!$("#themer-stylesheet").length)
		$('head').append('<style id="themer-stylesheet"></style>');

	var code = generateCSS(basePath);
	latestCode.less = code;

	$('#themer-stylesheet').attr('type', 'text/x-less').text(code);
	less.refreshStyles();

	if ($('.knob').length) $('.knob').attr('data-fgcolor', themerPrimaryColor);

	/*
	if (themerCustom[themerSelectedTheme].headerColor != null)
	{
		var tmc = rgb2hex(rgbString2obj($('.col.main-left').css('backgroundColor')));
		if (tmc != themerThemes[themerSelectedTheme].menuColor) updateMenuColor(tmc, false);
		console.log(tmc);
	}
	*/

	//console.log(generateCSS(basePath));

	//parser = new less.Parser({});
	//parser.parse(generateCSS(basePath), function (error, root) {
	 //   console.log( root.toCSS() );
	   // console.log(error);
	//});
}

function updateCharts()
{
	if (typeof charts == 'undefined')
		return false;

	//console.log('before: ' + charts.utility.chartColors);

	// apply styling
	charts.utility.chartColors.shift();
	charts.utility.chartColors.unshift(themerPrimaryColor);

	//console.log('after: ' + charts.utility.chartColors);

	if (typeof charts.website_traffic_graph != 'undefined' && charts.website_traffic_graph.plot != null)
		charts.website_traffic_graph.init();

	if (typeof charts.website_traffic_overview != 'undefined' && charts.website_traffic_overview.plot != null)
		charts.website_traffic_overview.init();

	if (typeof charts.traffic_sources_pie != 'undefined' && charts.traffic_sources_pie.plot != null)
		charts.traffic_sources_pie.init();

	if (typeof charts.chart_simple != 'undefined' && charts.chart_simple.plot != null)
		charts.chart_simple.init();

	if (typeof charts.chart_lines_fill_nopoints != 'undefined' && charts.chart_lines_fill_nopoints.plot != null)
		charts.chart_lines_fill_nopoints.init();

	if (typeof charts.chart_ordered_bars != 'undefined' && charts.chart_ordered_bars.plot != null)
		charts.chart_ordered_bars.init();

	if (typeof charts.chart_donut != 'undefined' && charts.chart_donut.plot != null)
		charts.chart_donut.init();

	if (typeof charts.chart_stacked_bars != 'undefined' && charts.chart_stacked_bars.plot != null)
		charts.chart_stacked_bars.init();

	if (typeof charts.chart_pie != 'undefined' && charts.chart_pie.plot != null)
		charts.chart_pie.init();

	if (typeof charts.chart_horizontal_bars != 'undefined' && charts.chart_horizontal_bars.plot != null)
		charts.chart_horizontal_bars.init();

	if (typeof charts.chart_live != 'undefined' && charts.chart_live.plot != null)
		charts.chart_live.init();
}

function updateTheme(themeSelect)
{
	if ($('#themer-theme').val() != themeSelect) $('#themer-theme').val(themeSelect);

	themerSelectedTheme = themeSelect; // index
	$.cookie('themerSelectedTheme', themerSelectedTheme);

	var uPrimaryColor = themerCustom[themeSelect].primaryColor != null ? themerCustom[themeSelect].primaryColor : themerThemes[themeSelect].primaryColor;
	var uHeaderColor = themerCustom[themeSelect].headerColor != null ? themerCustom[themeSelect].headerColor : themerThemes[themeSelect].headerColor;
	var uMenuColor = themerCustom[themeSelect].menuColor != null ? themerCustom[themeSelect].menuColor : themerThemes[themeSelect].menuColor;

	updatePrimaryColor(uPrimaryColor);
	updateHeaderColor(uHeaderColor);
	updateMenuColor(uMenuColor, false, true);  // attach style, apply charts

	if (themeSelect == 0 && themerCustom[themeSelect].primaryColor == null && themerCustom[themeSelect].headerColor == null && themerCustom[themeSelect].menuColor == null)
		attachStylesheet('', true); // reset
	else
		attachStylesheet();
}

function themerGetCode(less)
{
	var tlc;
	if (less === true)
		tlc = latestCode.less;
	else
		tlc = latestCode.css();

	//bootbox.alert($('<textarea class="input-block-level" rows="10"></textarea>').val(tlc));
	bootbox.alert($('<pre class="prettyprint lang-html" id="themer-pretty"></pre>').html(tlc));
}

var primaryBgColorTargets =
[
	".widget .widget-head",
	".btn-primary",
	".navbar.main .topnav > li .dropdown-menu li > a:hover, .navbar.main .topnav > li .dropdown-menu .active > a, .navbar.main .topnav > li .dropdown-menu .active > a:hover",
	"#flotTip",
	".btn-group.open .btn-primary.dropdown-toggle, .btn-primary.disabled, .btn-primary[disabled], .btn-primary:hover",
	".filter-bar div.lbl",
	".widget.widget-2.primary .widget-head",
	".widget .widget-body.list.list-2 ul li.active a i:before",
	".label-important",
	".table-primary thead th",
	".pagination ul > .active > a, .pagination ul > .active > span",
	".gallery ul li .thumb"
];
var primaryTextColorTargets =
[
 	"a, p a",
	".col.main-left ul li.active.glyphicons i:before",
	".col.main-left ul li.active a",
	".widget .widget-body.list ul li .count",
	".breadcrumb a, .breadcrumb .glyphicons i:before, .breadcrumb .glyphicons",
	".navbar.main .profile h1",
	".col.main-left .navarrow span i:before",
	".forgot, .forgot i:before",
	".glyphicons.single, .glyphicons.single i:before",
	".widget .widget-body.list.list-2 ul li.active a",
	".table-primary tbody td.important",
	".finances_cashflow .glyphicons.btn-action.single i:before",
	".col.main-left ul li.hasSubmenu2 ul.collapse li.active a i:before"
];
var primaryBorderColorTargets =
[
	".col.main-left ul li.active",
	".widget .widget-head",
	".btn-primary",
	".navbar.main .notif li a",
	"#flotTip",
	".widget.widget-2.primary .widget-head",
	".widget .widget-body.list.list-2 ul li.active a i:before",
	".table-primary thead th",
	".pagination ul > .active > a, .pagination ul > .active > span"
];
var headerBgColorTargets =
[
	".navbar.main",
	".navbar.main .profile",
	".navbar.main .topnav > li"
];
var headerBorderColorTargets =
[
 	".navbar.main .profile",
 	".navbar.main .topnav > li",
 	".navbar.main .profile img.avatar",
 	".navbar.main .innerpx"
];
var headerBorderColorMixedTargets =
[
 	".navbar.main .line",
 	".loginWrapper .line"
];
var headerBoxShadowColorTargets =
[
 	".navbar.main .topnav > li"
];
var headerTextColorTargets =
[
 	".navbar.main .topnav > li > a span",
 	".navbar.main .topnav > li > .glyphicons i:before",
 	".navbar.main .profile strong"
];
var sidebarTextColorTargets =
[
 	".col.main-left ul li a",
 	".col.main-left ul li.glyphicons i:before",
 	".col.main-left ul li .glyphicons i:before"
];
var headerCaretColorTargets =
[
 	".navbar.main .topnav > li .btn-group .caret"
];
var sidebarBgColorTargets =
[
 	".col.main-left"
];
var sidebarBgColor2Targets =
[
 	".col.main-left ul li.hasSubmenu2 ul.collapse li"
];
var sidebarBorderColorTargets =
[
 	".col.main-left ul li",
];
var sidebarBorderColor2Targets =
[
 	".col.main-left ul li > a",
 	".col.main-left ul li.hasSubmenu2 ul"
];
var contentBgColorTargets =
[
 	".span12.main",
 	".col.main-left ul li.active, .col.main-left ul li.hasSubmenu2 ul.collapse li.active"
];

/*
 * Persistent Selected Theme
 */
var themerSelectedTheme = $.cookie('themerSelectedTheme') != null ? $.cookie('themerSelectedTheme') : 0;

/*
 * Holds the latest CSS/LESS
 */
var latestCode = {
	css: function(){ return $('#themer-stylesheet').text(); },
	less: null
};

var themerMenuPositions = [
     { name: 'Left', className: 'left-menu' },
     { name: 'Right', className: 'right-menu' },
     { name: 'Top', className: 'top-menu' }
];

var themerMenuSizes = [
	{ name: 'Large' },
	{ name: 'Small' }
];

var themerThemes = [
	{
		name: "Default",
		primaryColor: "#1f79b7",
		headerColor: "#ffffff",
		menuColor: "#1f79b7",
		visible: true
	},
	{
		name: "Brown",
		primaryColor: "#ba5d32",
		headerColor: "#402a21",
		menuColor: "#855843",
		visible: true
	},
	{
		name: "Purple-Gray",
		primaryColor: "#86618f",
		headerColor: "#3d3d3d",
		menuColor: "#dedde0",
		visible: true
	},
	{
		name: "Purple-Wine",
		primaryColor: "#b94b6f",
		headerColor: "#2b2b36",
		menuColor: "#393947",
		visible: true
	},
	{
		name: "Blue-Gray",
		primaryColor: "#496cad",
		headerColor: "#d2d8db",
		menuColor: "#37373d",
		visible: true
	},
	{
		name: "Green Army",
		primaryColor: "#6f8745",
		headerColor: "#252b25",
		menuColor: "#353b32",
		visible: true
	},
	{
		name: "Black & White",
		primaryColor: "#575757",
		headerColor: "#d4d4d4",
		menuColor: "#292929",
		visible: true
	},
	{
		name: "Army",
		primaryColor: "#7a7a3a",
		headerColor: "#ededdc",
		menuColor: "#ededdc",
		visible: false
	},
	{
		name: "Evil Army",
		primaryColor: "#567a3a",
		headerColor: "#2b3830",
		menuColor: "#2b3830",
		visible: false
	},
	{
		name: "Forest",
		primaryColor: "#947131",
		headerColor: "#ededdc",
		menuColor: "#ededdc",
		visible: false
	},
	{
		name: "Cold Blue",
		primaryColor: "#676d8a",
		headerColor: "#edf2f5",
		menuColor: "#edf2f5",
		visible: false
	},
	{
		name: "Warm Blue",
		primaryColor: "#cc5470",
		headerColor: "#edf2f5",
		menuColor: "#edf2f5",
		visible: false
	},
	{
		name: "Experiment #2",
		primaryColor: "#438080",
		headerColor: "#fcf9f9",
		menuColor: "#fcf9f9",
		visible: false
	}
];

/*
 * Persistent Custom Theme Colors
 */
var themerCustomDefault = [];
$.each(themerThemes, function(k,v) { themerCustomDefault[k] = { primaryColor: null, headerColor: null, menuColor: null }; });
var themerCustom = $.cookie('themerCustom') != null ? $.parseJSON($.cookie('themerCustom')) : themerCustomDefault;

if (themerThemes.length != themerCustom.length)
{
	$.each(themerThemes, function(k,v){ if (typeof themerCustom[k] == 'undefined') themerCustom[k] = v; });
	$.cookie('themerCustom', JSON.stringify(themerCustom));
}

$(function()
{
	if ($('#themer').length)
	{
		$("#themer-primary-cp")
			.attr('data-default', themerPrimaryColor)
			.on('change', function(){
				var input = $(this),
				hex = input.val();
				if (hex) updatePrimaryColor(hex, true, true);
			});

		$("#themer-header-cp")
			.attr('data-default', themerHeaderColor)
			.on('change', function(){
				var input = $(this),
				hex = input.val();
				if (hex) updateHeaderColor(hex, true, false);
			});

		$("#themer-menu-cp")
			.attr('data-default', themerMenuColor)
			.on('change', function(){
				var input = $(this),
				hex = input.val();
				if (hex) updateMenuColor(hex, true, false);
			});

		var themeSelect = $('#themer-theme');
		$.each(themerThemes, function( i, p ) {
			if (p.visible === true)
			{
				var option = $("<option></option>").text(p.name).val(i);
				themeSelect.append(option);
			}
		});
		themeSelect.on('change', function(e)
		{
			e.preventDefault();
			updateTheme(themeSelect.val());
		});

		$('#themer-getcode-less').click(function(e){
			e.preventDefault();
			themerGetCode(true);
		});

		$('#themer-getcode-css').click(function(e){
			e.preventDefault();
			themerGetCode();
		});

		$('#themer-custom-reset').click(function()
		{
			themerCustom[themerSelectedTheme].primaryColor = null;
			themerCustom[themerSelectedTheme].headerColor = null;
			themerCustom[themerSelectedTheme].menuColor = null;

			$.cookie('themerCustom', JSON.stringify(themerCustom));
			updateTheme(themerSelectedTheme);
		});

		$('#themer-advanced-toggle').on('change', function()
		{
			$.cookie('themerAdvanced', $(this).is(':checked') ? "1" : "0");
			themerAdvancedToggle();
		});

		if (themerAdvanced)
			$('#themer-advanced-toggle').prop('checked', true).trigger('change');

		updateTheme(themerSelectedTheme);
	}
});
