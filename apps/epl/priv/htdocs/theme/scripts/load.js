if (typeof active_menu == 'undefined') { var active_menu = ''; }

if (typeof skin == 'undefined') { var skin = ''; }

if (typeof JSON == 'undefined')
{
	var JSON = JSON || {};
	//implement JSON.stringify serialization
	JSON.stringify = JSON.stringify || function(obj) {
		var t = typeof (obj);
		if (t != "object" || obj === null) {
			// simple data type
			if (t == "string")
				obj = '"' + obj + '"';
			return String(obj);
		} else {
			// recurse array or object
			var n, v, json = [], arr = (obj && obj.constructor == Array);
			for (n in obj) {
				v = obj[n];
				t = typeof (v);
				if (t == "string")
					v = '"' + v + '"';
				else if (t == "object" && v !== null)
					v = JSON.stringify(v);
				json.push((arr ? "" : '"' + n + '":') + String(v));
			}
			return (arr ? "[" : "{") + String(json) + (arr ? "]" : "}");
		}
	};
}

/*
function scrollTo(id)
{
	$('html,body').animate({scrollTop: $("#"+id).offset().top},'slow');
}
*/

var MENU_JS,
	colMainLeftScroller,
	menuPosition,
	refreshScrollers,
	menuSize = null;

$(function()
{
	// allow menu customization from browser
	MENU_JS = $('#themer-menu-position').length || $('#themer-menu-size').length || $('[data-toggle="menu-position"]').length || $('[data-toggle="menu-size"]').length;

	refreshScrollers = function()
	{
		if (typeof iScroll == 'undefined') return false;
		$.each(scrollers, function(k,v){
			v.refresh();
		});
		if (typeof scrollers.mainYScroller == 'object') scrollers.mainYScroller.refresh();
		if (typeof scrollers.colMainLeftScroller == 'object') scrollers.colMainLeftScroller.refresh();
	}

	var colMainLeft = $('.col.main-left');
	var colMainLeftWidth = '14.893617021276595%';
	var colMainLeftTargetWidth = colMainLeft.is('.login') ? '100%' : colMainLeftWidth;
	var colMainRight = $('.col.main-right');

	/*
	 * Menu size
	 */
	menuSize = $.cookie('menuSize') != null ? $.cookie('menuSize') : 0; // 0 = default / 1 = small
	if (!MENU_JS) menuSize = colMainLeft.is('.menu-small') ? 1 : 0;

	/*
	 * Menu scroller
	 */
	function updateMenuScrollerHorizontal()
	{
		if (typeof iScroll == 'undefined') return false;

		var sum = 0;
		$('.col.main-left div.iScrollWrapper > ul > li').each( function(){ sum += $(this).outerWidth(true); });
		$('.col.main-left .scroll-y-left div.iScrollWrapper').width(sum);

		if (colMainLeftScroller != null)
		{
			// destroy existing iScroll
			colMainLeftScroller.destroy();
			colMainLeftScroller = null;

			// remove from public scrollers array
			scrollers['colMainLeftScroller'] = null;
		}
		colMainLeftScroller = new iScroll($('.col.main-left .scroll-y-left').get(0), {
			hScrollbar: false,
            vScrollbar: false,
            hScroll: true,
            vScroll: false,
			momentum: true,
			wheelHorizontal: true,
            lockDirection: true,
            useTransform: $('html').is('.lt-ie9') ? false : true,
			onScrollEnd: function()
			{
				//toggleNav();
				//console.log(tMatrixY + ' maxY:' + maxY);
			},
			onBeforeScrollStart: function (e)
			{
				var target;
				if (!e) var e = window.event;
				if (e.target) target = e.target;
				else if (e.srcElement) target = e.srcElement;
				if (target.nodeType == 3) target = target.parentNode;

				if (target.tagName != 'SELECT' && target.tagName != 'INPUT' && target.tagName != 'TEXTAREA')
				{
					if (e.preventDefault) e.preventDefault();
					else e.returnValue = false;
				}
			}
		});
		scrollers['colMainLeftScroller'] = colMainLeftScroller;
	}

	function updateMenuScrollerVertical()
	{
		if (typeof iScroll == 'undefined') return false;

		$('.col.main-left .scroll-y-left div.iScrollWrapper').width('auto');
		//$('.col.main-left').addClass('hidden-phone');

		if (colMainLeftScroller != null)
		{
			// destroy existing iScroll
			colMainLeftScroller.destroy();
			colMainLeftScroller = null;

			// remove from public scrollers array
			scrollers['colMainLeftScroller'] = null;
		}
		colMainLeftScroller = new iScroll($('.col.main-left .scroll-y-left').get(0), {
			hScroll: false,
			scrollbarClass: 'iScrollLeft',
			hideScrollbar: true,
			checkDOMChanges: true,
			snap: false,
			momentum: true,
			useTransform: $('html').is('.lt-ie9') ? false : true,
			onScrollEnd: function()
			{
				toggleNav();
				//console.log(tMatrixY + ' maxY:' + maxY);
			},
			onBeforeScrollStart: function (e)
			{
				var target;
				if (!e) var e = window.event;
				if (e.target) target = e.target;
				else if (e.srcElement) target = e.srcElement;
				if (target.nodeType == 3) target = target.parentNode;

				if (target.tagName != 'SELECT' && target.tagName != 'INPUT' && target.tagName != 'TEXTAREA')
				{
					if (e.preventDefault) e.preventDefault();
					else e.returnValue = false;
				}
			}
		});
		scrollers['colMainLeftScroller'] = colMainLeftScroller;
	}

	/*
	 * Menu position
	 */
	menuPosition = $.cookie('menuPosition') != null ? $.cookie('menuPosition') : 'top-menu';
	if (!MENU_JS && $('.container-fluid:first').is('.left-menu')) menuPosition = 'left-menu';
	if (!MENU_JS && $('.container-fluid:first').is('.right-menu')) menuPosition = 'right-menu';
	if (!MENU_JS && $('.container-fluid:first').is('.top-menu')) menuPosition = 'top-menu';
	if (!MENU_JS && menuPosition == 'top-menu' && menuSize == 1) menuTopToggleSize(1, true);

	function menuTogglePosition(p)
	{
		$('#footer [data-toggle="menu-position"]').parent().removeClass('active');
		$('#footer [data-menu-position="'+p+'"]').parent().addClass('active');

		var colMainLeftVisible = colMainLeft.is(':visible');
		var colMainLeftVisiblePhone = colMainLeft.is(':visible') && !colMainLeft.hasClass('hidden-phone');

		colMainLeft.animate({ width: 'hide' }, 200, function()
		{
			$('.container-fluid:first')
				.removeClass('left-menu')
				.removeClass('right-menu')
				.removeClass('top-menu')
				.addClass(p);

			menuPosition = p;
			$.cookie('menuPosition', menuPosition);

			if (p == 'top-menu')
			{
				var h = '70px';
				if (menuSize == 1) h = '45px';
				colMainLeft.width('100%').height(h);

				if (menuSize == 1)
					menuTopToggleSize(1, colMainLeftVisible);

				if (menuSize == 0)
					menuTopToggleSize(0, colMainLeftVisible);

				colMainLeft.show();
				updateMenuScrollerHorizontal();
			}
			else
			{
				//menuTopSmallEventsDestroy();
				var cmlH = '14.893617021276595%';
				//if (colMainLeft.is(':hidden')) cmlH = '100%';
				if (colMainLeftVisiblePhone === true) cmlH = '100%';

				$('.col.main-left').width(cmlH).height('auto');
				$('.col.main-right .inner.topRight').css('margin-top', 0);
				updateMenuScrollerVertical();

				colMainLeft.hide().animate({ width: 'show' }, 200);
			}
		});
	}

	/*
	 * Toggle menu position: Footer navbar
	 */
	$('#footer [data-toggle="menu-position"]').on('click', function(e)
	{
		e.preventDefault();
		var p = $(this).attr('data-menu-position');

		menuTogglePosition(p);
	})
	.parent().removeClass('active')
	.find('[data-menu-position="'+menuPosition+'"]')
	.parent().addClass('active');

	/*
	 * Toggle menu position: Themer options
	 */
	var themerMenuPositionSelect = $('#themer-menu-position');
	$.each(themerMenuPositions, function( i, p ) {
		var option = $('<option data-className="' + p.className + '"></option>').text(p.name).val(i);
		themerMenuPositionSelect.append(option);
	});
	themerMenuPositionSelect.on('change', function(e)
	{
		e.preventDefault();
		menuTogglePosition(themerMenuPositions[themerMenuPositionSelect.val()].className);
	});
	if (themerMenuPositionSelect.length)
		themerMenuPositionSelect.find('[data-className="'+menuPosition+'"]').prop('selected', true);

	function menuToggleSizeHelper(s)
	{
		$('#footer [data-toggle="menu-size"]').parent().removeClass('active');
		$('#footer [data-menu-size="'+s+'"]').parent().addClass('active');

		var colMainLeftVisible = colMainLeft.is(':visible');

		menuSize = s;
		$.cookie('menuSize', menuSize);

		if (menuPosition == 'top-menu' && menuSize == 1)
		{
			//menuTopSmallEvents();
			menuTopToggleSize(1, colMainLeftVisible);
		}
		if (menuPosition == 'top-menu' && menuSize == 0)
		{
			//menuTopSmallEventsDestroy();
			menuTopToggleSize(0, colMainLeftVisible);
		}
		if (menuPosition != 'top-menu')
			menuToggleSize(menuSize);
	}

	/*
	 * Toggle menu size: Footer navbar
	 */
	$('#footer [data-toggle="menu-size"]').on('click', function(e)
	{
		e.preventDefault();
		var s = $(this).attr('data-menu-size');

		menuToggleSizeHelper(s);
	})
	.parent().removeClass('active')
	.find('[data-menu-size="'+menuSize+'"]')
	.parent().addClass('active');

	/*
	 * Toggle menu size: Themer options
	 */
	var themerMenuSizeSelect = $('#themer-menu-size');
	$.each(themerMenuSizes, function( i, p ) {
		var option = $('<option data-className="' + p.className + '"></option>').text(p.name).val(i);
		themerMenuSizeSelect.append(option);
	});
	themerMenuSizeSelect.on('change', function(e)
	{
		e.preventDefault();
		menuToggleSizeHelper(themerMenuSizeSelect.val());
	});
	if (themerMenuSizeSelect.length)
		themerMenuSizeSelect.val(menuSize);

	/*
	 * Enable/Disable Themer menu options
	 */
	function themerMenuOptionsToggle(disable)
	{
		var o = [themerMenuPositionSelect, themerMenuSizeSelect];
		$.each(o, function(k,v)
		{
			v.prop('disabled', disable);
			if (disable === true) v.parent().tooltip({ placement: 'left', title: "Layout options in this section are disabled" });
		});
	}
	if (colMainLeft.is('.login'))
		themerMenuOptionsToggle(true);

	function toggleNav()
	{
		if (colMainLeft.is('.login')) return false;
		var iScrollWrapperTransform = colMainLeft.find('.scroll-y-left div.iScrollWrapper').css('transform');

		if (typeof iScrollWrapperTransform == 'undefined') return false;

		var tMatrix = iScrollWrapperTransform.match(/[0-9-\.]+/g),
			tMatrixY = false,
			maxY = colMainLeft.find('.scroll-y-left').height(),
			actH = colMainLeft.find('.scroll-y-left div.iScrollWrapper').height(),
			navarrow = colMainLeft.find('.navarrow'),
			maxtMatrixYRange = colMainLeft.find('ul li:last').height(),
			maxtMatrixY = (Math.abs(actH - maxY - maxtMatrixYRange) * -1);

		if (tMatrix != null)
			tMatrixY = tMatrix[5];

		if (actH > maxY)
		{
			//console.log('show');
			navarrow.show();
		}
		else
		{
			//console.log('hide');
			navarrow.hide();
		}

		if ((tMatrixY && tMatrixY == 0 && actH > maxY) || !tMatrixY)
			navarrow.show();

		if (tMatrixY <= maxtMatrixY)
			navarrow.hide();
	}

	function menuToggleSize(size)
	{
		if (size == 0)
			colMainLeft.removeClass('menu-small');

		if (size == 1)
			colMainLeft.addClass('menu-small');

		updateMenuScrollerVertical();
	}

	function menuTopToggleSize(size, phone)
	{
		if (size == 0)
		{
			$('.col.main-left').stop().animate({ height: '70px' }, 200, function(){ $(this).removeClass('menu-small'); updateMenuScrollerHorizontal(); });
			if (phone === true) $('.col.main-right .inner.topRight').stop().animate({ marginTop: '70px' }, 200, function(){ refreshScrollers(); });

			/*
			$('.col.main-left').stop().animate({ height: '70px' }, 200, function(){
				$('.top-menu .col.main-left ul li > .glyphicons i, .top-menu .col.main-left ul li.glyphicons i').css({ paddingTop: '14px' }, 500);
				$('.col.main-left ul > li a span').stop().hide().fadeIn();
			});
			$('.col.main-right .inner.topRight').stop().animate({ marginTop: '70px' }, 200); */
		}
		if (size == 1)
		{
			$('.col.main-left').stop().animate({ height: '45px' }, 200, function(){ $(this).addClass('menu-small'); updateMenuScrollerHorizontal(); });
			if (phone === true) $('.col.main-right .inner.topRight').stop().animate({ marginTop: '45px' }, 200, function(){ refreshScrollers(); });

			/*
			$('.col.main-left ul > li a span').stop().fadeOut(function(){
				$('.col.main-left').stop().animate({ height: '45px' }, 200, function(){});
				$('.col.main-right .inner.topRight').stop().animate({ marginTop: '45px' }, 200);
				$('.top-menu .col.main-left ul li > .glyphicons i, .top-menu .col.main-left ul li.glyphicons i').css({ paddingTop: '10px' }, 500);
			});*/
		}
	}

	function menuTopSmallEvents()
	{
		$(document).on('mouseenter', '.top-menu .col.main-left', function()
		{
			menuTopToggleSize(0);
		}).on('mouseleave', '.top-menu .col.main-left', function()
		{
			menuTopToggleSize(1);
		});
	}

	function menuTopSmallEventsDestroy()
	{
		$(document)
			.off('mouseenter', '.top-menu .col.main-left')
			.off('mouseleave', '.top-menu .col.main-left');
	}

	//if (menuPosition != 'top-menu')
	if (colMainLeft.is('.login') || (!colMainLeft.is('.login') && menuPosition != 'top-menu'))
	{
		colMainLeft.find('ul:first').hide();
		colMainLeft.css('width', '0px' ).removeClass('hide').animate({ width: colMainLeftTargetWidth }, 250, function()
		{
			//console.log('test');
			if (colMainLeft.is('.login'))
			{
				$('.navbar.main').find('.profile').css('float', 'none');
				$('.navbar.main').find('.positionWrapper')
					.css('textAlign', 'center')
					.width($('.navbar.main .profile').width()+30)
					.animate({ width: '100%' }, 1500, function(){

						$('.positionWrapper.loginWrapper')
							.appendTo('.col.main-left .rrow div')
							.css({
								position: 'absolute',
								left: $(document).width()/2 - $('.positionWrapper.loginWrapper').width()/2,
								top: -$('.positionWrapper.loginWrapper').height(),
								display: 'block'
							})
							.animate({
								top: 0,
								paddingTop: ($('.col.main-left .rrow').height()*0.05),
								paddingBottom: ($('.col.main-left .rrow').height()*0.05) + $('.btn-register').height()
							}, 1000, function(){
								$(this).parents('div:first').height($(this).outerHeight());
								refreshScrollers();
							});

					});

				colMainRight.css({
					width: '100%',
					left: 0,
					marginLeft: 0
				});

				$('.navbar.main .btn-navbar').addClass('hide');

			}
			else
				$(this).find('ul:first').fadeIn();
		});
	}
	if (menuPosition == 'top-menu')
	{
		colMainLeft.removeClass('hide');
	}

	if (MENU_JS) $('.container-fluid:first').addClass(menuPosition);
	if (!colMainLeft.is(':visible') && menuPosition == 'top-menu') $('.col.main-right .inner.topRight').css({ marginTop: 0 });
	if (!colMainLeft.is('.login')) $('#footer').show(1000);

	if (typeof iScroll != 'undefined' && !colMainLeft.is('.login'))
	{
		if (menuPosition == 'top-menu')
		{
			updateMenuScrollerHorizontal();
			if (menuSize == 1)
			{
				menuTopToggleSize(1);
				//menuTopSmallEvents();
			}
		}
		else
		{
			menuToggleSize(menuSize);
			updateMenuScrollerVertical();
			toggleNav();
		}

		// navarrow scroll
		colMainLeft
			.find('.navarrow').on('click', function()
			{
				if (colMainLeft.find('li.currentScroll').next('li').length)
					colMainLeft.find('li.currentScroll').removeClass('currentScroll').next('li').addClass('currentScroll');

				colMainLeftScroller.scrollToElement('li.currentScroll', 300);
			});
	}

	$('.navbar.main .btn-navbar').click(function()
	{
		if (colMainLeft.is(':hidden'))
		{
			if (menuPosition == 'top-menu')
			{
				colMainLeft.stop().removeClass('hidden-phone');

				if (menuSize == 1)
					menuTopToggleSize(1, true);

				if (menuSize == 0)
					menuTopToggleSize(0, true);

				updateMenuScrollerHorizontal();
			}
			else
			{
				colMainLeft.stop().css('width', '0px').removeClass('hidden-phone').animate({ width: '100%' }, 250, function()
				{
					$(window).resize();
				});
			}

			/* if (menuPosition == 'top-menu')
				$('.col.main-right .inner.topRight').animate({ marginTop: colMainLeft.height() }); */
		}
		else
		{
			colMainLeft.stop().animate({ width: '0px' }, 250, function(){
				$(this)
					.addClass('hidden-phone')
					.attr('style', '');

				$(window).resize();
			});
			if (menuPosition == 'top-menu')
				$('.col.main-right .inner.topRight').animate({ marginTop: 0 });
		}
	});

	var registerInit = false;
	$('.btn-register a').click(function(e)
	{
		e.preventDefault();
		colMainLeft.stop().animate({ width: '0px' }, 250, function()
		{
			$(this).addClass('hidden-phone');
			if (!registerInit)
			{
				$('.positionWrapper.registerWrapper')
					.css({
						position: 'absolute',
						left: $(document).width()/2 - $('.positionWrapper.registerWrapper').width()/2,
						top: -$('.positionWrapper.registerWrapper').height()
					})
					.removeClass('hide')
					.animate({
						top: 0,
						paddingTop: ($('.col.main-right .rrow').height()*0.05),
						paddingBottom: ($('.col.main-right .rrow').height()*0.05) + $('.btn-login').height()
					}, 1000, function(){
						$(this).parents('div:first').height($(this).outerHeight());
						$(window).resize();
						registerInit = true;
					});

				$('.navbar.main .btn-navbar').removeClass('hide');
			}
		});
	});
	$('.btn-login a').click(function(e)
	{
		e.preventDefault();
		colMainLeft.stop().css('width', '0px').removeClass('hidden-phone').animate({ width: '100%' }, 250, function(){
			refreshScrollers();
			$(window).resize();
		});
	});

	var allowSubmit = false;
	$('form.fts').submit(function(e)
	{
		if (allowSubmit) return true;

		e.preventDefault();
		var fts = $(this);
		$('.navbar.main').find('*').fadeOut(500);
		colMainRight.find('*').hide();
		colMainLeft.animate({ width: 0 }, 500, function()
		{
			allowSubmit = true;
			fts.submit();
		});
	});

	colMainLeft.find('ul a').not('[data-toggle]').click(function(e)
	{
		e.preventDefault();
		var eLocation = $(this).attr('href');
		$('#plugin_content').load(eLocation);

		if (active_menu != '') {
			active_menu._removeClass('active');
			active_menu._removeClass('currentScroll');
		}

		active_menu = $(this).parent();
		active_menu._addClass('active');
		active_menu._addClass('currentScroll');
	});

	colMainLeft
		.find('ul.collapse')
		.on('shown', function(e)
		{
			refreshScrollers();
			//colMainLeftScroller.scrollToElement('li.currentScroll', 0, true);
		})
		.on('hidden', function(e)
		{
			if (typeof iScroll != 'undefined')
			{
				if (menuPosition != 'top-menu')
					setTimeout(function(){ refreshScrollers(); colMainLeftScroller.scrollToElement('li.currentScroll', 200); }, 0);
				else
					refreshScrollers();
			}
		})
		.on('hide', function(e)
		{
			// preserve current scroll & move to the opened collapsible
			if ($(this).parent().is('.currentScroll'))
			{
				colMainLeft.find('.currentScroll').removeClass('currentScroll');
				colMainLeft.find('.oldCurrentScroll').addClass('currentScroll').removeClass('oldCurrentScroll');

				if (menuPosition != 'top-menu')
					colMainLeft.find('ul:first-child > li').not(':visible').slideDown();
				else
					colMainLeft.find('ul:first-child > li').not(':visible').animate({ width: 'show' });

				// reopened old submenu
				colMainLeft.find('.currentScroll ul.collapse').addClass('in').height('auto');
			}
		})
		.on('show', function(e)
		{
			//alert('show');

			// preserve current scroll & move to the opened collapsible
			colMainLeft.find('.currentScroll').addClass('oldCurrentScroll').removeClass('currentScroll');
			// close opened submenus
			colMainLeft.find('ul.collapse.in').removeClass('in').height(0);
			$(this).parent().addClass('currentScroll');

			if (menuPosition != 'top-menu')
				colMainLeft.find('ul:first-child > li').not('.currentScroll').slideUp(200);
			else
				colMainLeft.find('ul:first-child > li').not('.currentScroll').animate({ width: 'hide' });
		});

	var notifShowInt = 600;
	$('.navbar.main .notif li').hide().each(function(k,v){
		setTimeout(function(){
			$(v).show().css('opacity', '0').stop().animate({ opacity: 100 }, 2000);
		},(k+1)*notifShowInt);
	});

	$("[rel='tooltip']").tooltip();
	$("[rel='popover']").popover();

	$(window).resize(function()
	{
		refreshScrollers();

		if (colMainLeft.is('.login'))
		{
			if (colMainLeft.is(':visible'))
			{
				$('.positionWrapper.loginWrapper')
					.css({
						left: $(document).width()/2 - $('.positionWrapper.loginWrapper').width()/2,
						paddingTop: ($('.col.main-left .rrow').height()*0.05),
						paddingBottom: ($('.col.main-left .rrow').height()*0.05) + $('.btn-register').height()
					})
					.parents('div:first').height($('.positionWrapper.loginWrapper').outerHeight());

				refreshScrollers();
			}

			if (registerInit && (!colMainLeft.is(':visible') || colMainLeft.width() == 0))
			{
				$('.positionWrapper.registerWrapper')
					.css({
						left: $(document).width()/2 - $('.positionWrapper.registerWrapper').width()/2,
						paddingTop: ($('.col.main-right .rrow').height()*0.05),
						paddingBottom: ($('.col.main-right .rrow').height()*0.05) + $('.btn-login').height()
					}).parents('div:first').height($('.positionWrapper.registerWrapper').outerHeight());

				refreshScrollers();
			}
		}

		if (colMainLeft.is(':hidden') && menuPosition == 'top-menu' && parseInt($('.col.main-right .inner.topRight').css('marginTop')) != 0)
			$('.col.main-right .inner.topRight').css({ marginTop: 0 });

		if (colMainLeft.is(':visible') && !colMainLeft.is('.login') && menuPosition == 'top-menu' && parseInt($('.col.main-right .inner.topRight').css('marginTop')) != parseInt(colMainLeft.height()))
			$('.col.main-right .inner.topRight').css({ marginTop: colMainLeft.height() });
	});

	$('.disable-click').click(function(e){ e.preventDefault(); });

	// make sure we can scroll
	setTimeout(function(){
		refreshScrollers();
	},1000);

	// jump to active menu
	if (menuPosition != 'top-menu')
	{
		setTimeout(function(){
			if (typeof iScroll != 'undefined' &&
				$('.main-left li').index($('.main-left .currentScroll')) > 1)
					colMainLeftScroller.scrollToElement('li.currentScroll', 300);
		}, 1000);
	}

	// refresh scrollers when tabs are shown
	$('a[data-toggle="tab"]').on('shown', function (e) {
		refreshScrollers();
	});

	/* wysihtml5 */
	if ($('textarea.wysihtml5').size() > 0)
		$('textarea.wysihtml5').wysihtml5();

	/* DataTables */
	if ($('.dynamicTable').size() > 0)
	{
		$('.dynamicTable').dataTable({
			"sPaginationType": "bootstrap",
			"sDom": "<'row-fluid'<'span6'l><'span6'f>r>t<'row-fluid'<'span6'i><'span6'p>>",
			"oLanguage": {
				"sLengthMenu": "_MENU_ records per page"
			}
		});
	}

	/*
	 * Helper function for JQueryUI Sliders Create event
	 */
	function JQSliderCreate()
	{
		$(this)
			.removeClass('ui-corner-all ui-widget-content')
			.wrap('<span class="ui-slider-wrap"></span>')
			.find('.ui-slider-handle')
			.removeClass('ui-corner-all ui-state-default');
	}

	/*
	 * JQueryUI Slider: Default slider
	 */
	if ($('.slider-single').size() > 0)
	{
		$( ".slider-single" ).slider({
			create: JQSliderCreate,
			value: 10,
	        animate: true,
	        start: function() { if (typeof mainYScroller != 'undefined') mainYScroller.disable(); },
	        stop: function() { if (typeof mainYScroller != 'undefined') mainYScroller.enable(); }
	    });
	}

	/*
	 * JQueryUI Slider: Multiple Vertical Sliders
	 */
	$( ".sliders-vertical > span" ).each(function()
	{
        var value = parseInt( $( this ).text(), 10 );
        $( this ).empty().slider({
        	create: JQSliderCreate,
            value: value,
            range: "min",
            animate: true,
            orientation: "vertical",
            start: function() { if (typeof mainYScroller != 'undefined') mainYScroller.disable(); },
	        stop: function() { if (typeof mainYScroller != 'undefined') mainYScroller.enable(); }
        });
    });

	/*
	 * JQueryUI Slider: Range Slider
	 */
	if ($('.range-slider').size() > 0)
    {
		$( ".range-slider .slider" ).slider({
			create: JQSliderCreate,
	        range: true,
	        min: 0,
	        max: 500,
	        values: [ 75, 300 ],
	        slide: function( event, ui ) {
	            $( ".range-slider #amount" ).val( "$" + ui.values[ 0 ] + " - $" + ui.values[ 1 ] );
	        },
	        start: function() { if (typeof mainYScroller != 'undefined') mainYScroller.disable(); },
	        stop: function() { if (typeof mainYScroller != 'undefined') mainYScroller.enable(); }
	    });
    	$( ".range-slider #amount" ).val( "$" + $( ".range-slider .slider" ).slider( "values", 0 ) +
    			" - $" + $( ".range-slider .slider" ).slider( "values", 1 ) );
    }

	/*
	 * JQueryUI Slider: Snap to Increments
	 */
	if ($('.increments-slider').size() > 0)
    {
		$( ".increments-slider .slider" ).slider({
			create: JQSliderCreate,
			value:100,
	        min: 0,
	        max: 500,
	        step: 50,
	        slide: function( event, ui ) {
	            $( ".increments-slider #amount" ).val( "$" + ui.value );
	        },
	        start: function() { if (typeof mainYScroller != 'undefined') mainYScroller.disable(); },
	        stop: function() { if (typeof mainYScroller != 'undefined') mainYScroller.enable(); }
	    });
		$( ".increments-slider #amount" ).val( "$" + $( ".increments-slider .slider" ).slider( "value" ) );
    }

	/*
	 * JQueryUI Slider: Vertical Range Slider
	 */
	if ($('.vertical-range-slider').size() > 0)
    {
		$( ".vertical-range-slider .slider" ).slider({
			create: JQSliderCreate,
			orientation: "vertical",
	        range: true,
	        min: 0,
	        max: 500,
	        values: [ 100, 400 ],
	        slide: function( event, ui ) {
	            $( ".vertical-range-slider #amount" ).val( "$" + ui.values[ 0 ] + " - $" + ui.values[ 1 ] );
	        },
	        start: function() { if (typeof mainYScroller != 'undefined') mainYScroller.disable(); },
	        stop: function() { if (typeof mainYScroller != 'undefined') mainYScroller.enable(); }
	    });
    	$( ".vertical-range-slider #amount" ).val( "$" + $( ".vertical-range-slider .slider" ).slider( "values", 0 ) +
    			" - $" + $( ".vertical-range-slider .slider" ).slider( "values", 1 ) );
    }

	/*
	 * JQueryUI Slider: Range fixed minimum
	 */
	if ($('.slider-range-min').size() > 0)
	{
		$( ".slider-range-min .slider" ).slider({
			create: JQSliderCreate,
            range: "min",
            value: 150,
            min: 1,
            max: 700,
            slide: function( event, ui ) {
                $( ".slider-range-min #amount" ).val( "$" + ui.value );
            },
            start: function() { if (typeof mainYScroller != 'undefined') mainYScroller.disable(); },
	        stop: function() { if (typeof mainYScroller != 'undefined') mainYScroller.enable(); }
        });
        $( ".slider-range-min #amount" ).val( "$" + $( ".slider-range-min .slider" ).slider( "value" ) );
	}

	/*
	 * JQueryUI Slider: Range fixed maximum
	 */
	if ($('.slider-range-max').size() > 0)
	{
		$( ".slider-range-max .slider" ).slider({
			create: JQSliderCreate,
            range: "max",
            min: 1,
            max: 700,
            value: 150,
            slide: function( event, ui ) {
                $( ".slider-range-max #amount" ).val( "$" + ui.value );
            },
            start: function() { if (typeof mainYScroller != 'undefined') mainYScroller.disable(); },
	        stop: function() { if (typeof mainYScroller != 'undefined') mainYScroller.enable(); }
        });
        $( ".slider-range-max #amount" ).val( "$" + $( ".slider-range-max .slider" ).slider( "value" ) );
	}

	/*
	 * Boostrap Extended
	 */
	// custom select for Boostrap using dropdowns
	$('.selectpicker').selectpicker();

	// bootstrap-toggle-buttons
	$('.toggle-button').toggleButtons();

	// dropdown hover
	//$('.dropdown-toggle').dropdownHover();

	/*
	 * UniformJS: Sexy form elements
	 */
	$('.uniformjs').find("select, input, button, textarea").uniform();

	// colorpicker
	if ($('#colorpicker').size() > 0)
	{
		$('#colorpicker').farbtastic('#colorpickerColor');
	}
	// datepicker
	if ($('#datepicker').length)
	{
		$("#datepicker").datepicker({
			showOtherMonths:true
		});
	}
	if ($('#datepicker-inline').length)
	{
		$('#datepicker-inline').datepicker({
	        inline: true,
			showOtherMonths:true
	    });
	}

	// bookings daterange
	if ($('#dateRangeFrom').length && $('#dateRangeTo').length)
	{
		$( "#dateRangeFrom" ).datepicker({
			defaultDate: "+1w",
			changeMonth: false,
			numberOfMonths: 2,
			onClose: function( selectedDate ) {
				$( "#dateRangeTo" ).datepicker( "option", "minDate", selectedDate );
			}
		}).datepicker( "option", "maxDate", $('#dateRangeTo').val() );

		$( "#dateRangeTo" ).datepicker({
			defaultDate: "+1w",
			changeMonth: false,
			numberOfMonths: 2,
			onClose: function( selectedDate ) {
				$( "#dateRangeFrom" ).datepicker( "option", "maxDate", selectedDate );
			}
		}).datepicker( "option", "minDate", $('#dateRangeFrom').val() );
	}

	$('.checkboxs thead :checkbox').change(function(){
		if ($(this).is(':checked'))
		{
			$('.checkboxs tbody :checkbox').prop('checked', true).parent().addClass('checked');
			$('.checkboxs tbody tr.selectable').addClass('selected');
			$('.checkboxs_actions').show();
		}
		else
		{
			$('.checkboxs tbody :checkbox').prop('checked', false).parent().removeClass('checked');
			$('.checkboxs tbody tr.selectable').removeClass('selected');
			$('.checkboxs_actions').hide();
		}
	});

	$('.checkboxs tbody').on('click', 'tr.selectable', function(e){
		var c = $(this).find(':checkbox');
		var s = $(e.srcElement);

		if (e.srcElement.nodeName == 'INPUT')
		{
			if (c.is(':checked'))
				$(this).addClass('selected');
			else
				$(this).removeClass('selected');
		}
		else if (e.srcElement.nodeName != 'TD' && e.srcElement.nodeName != 'TR' && e.srcElement.nodeName != 'DIV')
		{
			return true;
		}
		else
		{
			if (c.is(':checked'))
			{
				c.prop('checked', false).parent().removeClass('checked');
				$(this).removeClass('selected');
			}
			else
			{
				c.prop('checked', true).parent().addClass('checked');
				$(this).addClass('selected');
			}
		}
		if ($('.checkboxs tr.selectable :checked').size() == $('.checkboxs tr.selectable :checkbox').size())
			$('.checkboxs thead :checkbox').prop('checked', true).parent().addClass('checked');
		else
			$('.checkboxs thead :checkbox').prop('checked', false).parent().removeClass('checked');

		if ($('.checkboxs tr.selectable :checked').size() >= 1)
			$('.checkboxs_actions').show();
		else
			$('.checkboxs_actions').hide();
	});

	if ($('.checkboxs tbody :checked').size() == $('.checkboxs tbody :checkbox').size() && $('.checkboxs tbody :checked').length)
		$('.checkboxs thead :checkbox').prop('checked', true).parent().addClass('checked');

	if ($('.checkboxs tbody :checked').length)
		$('.checkboxs_actions').show();

	$('.radioboxs tbody tr.selectable').click(function(e){
		var c = $(this).find(':radio');
		if (e.srcElement.nodeName == 'INPUT')
		{
			if (c.is(':checked'))
				$(this).addClass('selected');
			else
				$(this).removeClass('selected');
		}
		else if (e.srcElement.nodeName != 'TD' && e.srcElement.nodeName != 'TR')
		{
			return true;
		}
		else
		{
			if (c.is(':checked'))
			{
				c.attr('checked', false);
				$(this).removeClass('selected');
			}
			else
			{
				c.attr('checked', true);
				$('.radioboxs tbody tr.selectable').removeClass('selected');
				$(this).addClass('selected');
			}
		}
	});

	// Knob
	if ($('.knob').length)
	{
		$('.knob').knob({
			draw: function()
			{
				if ($(this.i).attr('data-prepend'))
					$(this.i).val($(this.i).attr('data-prepend') + this.cv);
			}
		});
	}

	// sortable tables
	if ($( ".js-table-sortable" ).length)
	{
		$( ".js-table-sortable" ).sortable(
		{
			placeholder: "ui-state-highlight",
			items: "tbody tr",
			handle: ".js-sortable-handle",
			forcePlaceholderSize: true,
			helper: function(e, ui)
			{
				ui.children().each(function() {
					$(this).width($(this).width());
				});
				return ui;
			},
			start: function(event, ui)
			{
				if (typeof mainYScroller != 'undefined') mainYScroller.disable();
				ui.placeholder.html('<td colspan="' + $(this).find('tbody tr:first td').size() + '">&nbsp;</td>');
			},
		    stop: function() { if (typeof mainYScroller != 'undefined') mainYScroller.enable(); }
		});
	}
});
