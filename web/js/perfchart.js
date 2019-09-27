/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2019, VU University Amsterdam
			 CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/**
 * @fileOverview
 * Dynamic performance charts for monitoring memory usage, etc.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "flot", "utils", "form", "palette", "laconic" ],
       function($, config, plot, utils, form) {

(function($) {
  var pluginName = 'perfchart';

  var plot_default_options = {
    xaxis: {
      min: 0,
      max: 1000
    },
    yaxis: {
      min: 1,
      max: 1000000000000,
      ticks: [ 1,10,100,1000,10000,100000,
	       1000000,10000000,100000000,1000000000,
	       10000000000, 100000000000, 1000000000000
	     ],
      tickFormatter: suffixFormatter,
      transform: function(v) {return Math.log(v+1);}
    },
    grid: {
      hoverable: true
    }
  };

  /** @lends $.fn.perfchart */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */
	var hsplit;

	elem.data(pluginName, data);	/* store with element */
	elem.addClass("perfchart reactive-size swish-event-receiver listen-close-event");
	elem[pluginName]('controller');
	elem.append(hsplit=$($.el.div({class: "hsplit_lp"})));
	elem[pluginName]('series_menu');
	hsplit.append($.el.div({class:"flot"}));
	elem[pluginName]('tooltip');
	elem.on('reactive-resize', function() {
	  elem[pluginName]('resize');
	});
	elem.on('tab-close', function() {
	  if ( data.timer ) {
	    clearInterval(data.timer);
	    delete(data.timer);
	  }
	});
	elem.on('activate-tab', function() {
	  elem[pluginName]('redraw');
	});

	$.ajax({
	  url: config.http.locations.perf_series,
	  success: function(data) {
	    elem[pluginName]('series', data);
	  },
	  error: function(jqXHDR) {
	    modal.ajaxError(jqXHDR);
	  }
	});
      })
    },

    /**
     * Initialize the data series and flot instance from the reported
     * available series.
     */
    series: function(options) {
      var elem = $(this);
      var data = elem.data(pluginName);
      var colors = palette("mpn65", options.series.length);

      data.rate      = Math.round((options.rate||1)*1000);
      data.series    = {};
      data.x	     = 0;
      data.samples   = Math.round(elem.width()/2);
      data.flot_data = [];
      data.flot_opts = $.extend(true, {}, plot_default_options);

      data.flot_opts.xaxis.max = data.flot_opts.xaxis.min + data.samples;

      for(var i=0; i<options.series.length; i++) {
	var series = options.series[i];

	series.data  = [];
	if ( !series.label )
	  series.label = series.name;
	if ( !series.color )
	  series.color = "#"+colors[i];

	data.series[series.name] = series;
	if ( series.active ) {
	  var idx = data.flot_data.length;
	  series.index = idx;
	  data.flot_data.push(series);
	}
      }

      elem[pluginName]('series_menu');

      data.plot = $.plot(elem.find(".flot"), data.flot_data, data.flot_opts);
      data.plot.setupGrid();
      elem[pluginName]('update');
      elem[pluginName]('play', true);
    },

    activate_series: function(name, val) {
      var elem   = $(this);
      var data   = $(this).data(pluginName);
      var series = data.series[name];

      if ( val ) {
	var idx = data.flot_data.length;
	series.index = idx;
	data.flot_data.push(series);
      } else {
	data.flot_data.splice(series.index, 1);
	for(var i=0; i<data.flot_data.length; i++)
	  data.flot_data[i].index = i;
      }

      data.plot.setData(data.flot_data);
      data.plot.setupGrid();
      data.plot.draw();
    },

    /**
     * Start collecting the chart.
     * @param {Any} how is one of `false`, `true` or #milliseconds
     */
    play: function(how) {
      var elem  = $(this);
      var data  = $(this).data(pluginName);
      var clear = (how == false);

      if ( typeof how == "number" ) {
	data.rate = how;
	how = true;
	clear = true;
      }

      if ( clear && data.timer ) {
	clearInterval(data.timer);
	data.timer = null;
      }

      if ( how == true && !data.timer ) {
	data.timer = setInterval(function() {
	  elem[pluginName]('update');
	}, data.rate);
      }
    },

    controller: function() {
      var elem = $(this);
      var data = $(this).data(pluginName);
      var ctrl;
      var br;
      var play;
      var pause, clear;

      elem.append(ctrl=$($.el.div({class:"form-inline controller"})));
      ctrl.append(br=$($.el.div({class:"btn-group"})));
      br.append(clear=$(form.widgets.glyphIconButton("step-backward", {})),
		$("<span class='menu-space'>&nbsp</span>"),
		play =$(form.widgets.glyphIconButton("play", {})),
		pause=$(form.widgets.glyphIconButton("pause", {})));

      play.on("click", function()  { elem[pluginName]('play', true); });
      pause.on("click", function() { elem[pluginName]('play', false); });
      clear.on("click", function() { elem[pluginName]('clear'); });

      br.append($.el.label({class:"sample-rate"}, "Sample rate:"));
      br.append(sel=$($.el.select({class:"form-control"})));

      sel.on('change', function() {
	elem[pluginName]('play', this.value*1000);
      });

      function opt(time, label, def) {
	var opts = {"value":time};
	if ( def ) opts.selected = "selected";

	sel.append($.el.option(opts, label));
      }

      opt(0.2, "0.2 sec");
      opt(0.5, "0.5 sec");
      opt(1.0, "1 sec", true);
      opt(2.0, "2 sec");
      opt(5.0, "5 sec");
      opt(10.0, "10 sec");
      opt(60.0, "1 min");
      opt(120.0, "2 min");
      opt(300.0, "5 min");
    },

    series_menu: function() {
      var elem = $(this);
      var data = $(this).data(pluginName);
      var lbl = $.el.label("Series");
      var menu;

      if ( (menu=elem.find("div.series")) && menu.length > 0 ) {
	menu.empty().append(lbl);
      } else {
	elem.find(".hsplit_lp").append(menu=$($.el.div({class:"series"}, lbl)));
	menu.on("change", "input", function(ev) {
	  var name    = $(ev.target).attr('name');
	  var checked = $(ev.target).prop('checked') == true;

	  elem[pluginName]('activate_series', name, checked);
	});
      }

      if ( data.series ) {
	for(var p in data.series) {
	  if ( data.series.hasOwnProperty(p) ) {
	    var series = data.series[p];

	    menu.append(form.widgets.checkbox(series.name,
					      { checked: series.active,
						color: series.color,
						label: series.label,
						title: series.title
					      }));
	  }
	}
      }
    },

    /**
     * Enable the tooltip to get precise values
     */
    tooltip: function() {
      var data    = $(this).data(pluginName);
      var plotdiv = $(this).find(".flot");

      $(plotdiv).hover(function() { $("#flot-tooltip").show(); },
		       function() { $("#flot-tooltip").hide(); });
      $(plotdiv).on("plothover", function (event, pos, item) {
	var x = Math.round(pos.x);
	var t  = $.el.table();
	var hasval = 0;

	function val(ar, x) {
	  if ( ar[0] ) {
	    var x0 = ar[0][0];
	    var x1 = x-x0;
	    var mx = 5;

	    while(!(ar[x1] && ar[x1][0] == x) && --mx >= 0) {
	      if ( x1 >= ar.length )
		x1 = ar.length-1;
	      else if ( x1 < 0 )
		x1 = 0;
	      else
		x1 += x-ar[x1][0];
	    }

	    if ( mx > 0 )
	      return ar[x1][1];
	  }
	}

	$("#flot-tooltip").empty().append(t);

	for(var i=0; i<data.flot_data.length; i++) {
	  var series = data.flot_data[i];
	  var value, str;

	  if ( (value = val(series.data, x)) != undefined ) {
	    if ( series.unit == 'bytes' )
	      str = utils.human_size(value);
	    else
	      str = utils.human_count(value);
	    hasval++;
	  } else {
	    str = "n/a";
	  }

	  t.append($.el.tr($.el.th(series.label),
			   $.el.td(str)));
	}

	if ( hasval > 0 ) {
	  $("#flot-tooltip")
	      .css({top: pos.pageY+5, left: pos.pageX+5})
	      .show();
	} else {
	  $("#flot-tooltip").hide();
	}
      });
    },

    update: function() {
      var elem = $(this);

      $.ajax({
        url: config.http.locations.perf_sample,
	success: function(data) {
	  elem[pluginName]('add_sample', data);
	},
	error: function(jqXHDR) {
	  modal.ajaxError(jqXHDR);
	}
      });
    },

    /**
     * Add an individual sample
     */
    add_sample: function(options) {
      var elem   = $(this);
      var data   = elem.data(pluginName);
      var sample = options.sample||{};
      var x      = data.x++;
      var xaxis  = data.plot.getAxes().xaxis;
      var rm;

      if ( x > xaxis.options.max ) {
	rm = x - xaxis.options.max;
	xaxis.options.max += rm;
	xaxis.options.min += rm;
      } else
	rm = 0;

      for(var p in data.series) {
	if ( data.series.hasOwnProperty(p) ) {
	  var series = data.series[p];

	  if ( sample[p] ) {
	    if ( rm )
	      series.data.splice(0, rm);
	    series.data.push([x, sample[p]]);
	  }
	}
      }

      if ( elem.is(":visible") ) {
	data.plot.setData(data.flot_data);
	data.plot.setupGrid();			/* remove if grid is fixed */
	data.plot.draw();
      }
    },

    redraw: function() {
      var elem   = $(this);
      var data   = elem.data(pluginName);

      if ( elem.is(":visible") ) {
	data.plot.setData(data.flot_data);
	data.plot.resize();
	data.plot.setupGrid();			/* remove if grid is fixed */
	data.plot.draw();
      }
    },

    clear: function() {
      var elem  = $(this);
      var data  = elem.data(pluginName);

      for(var p in data.series) {
	if ( data.series.hasOwnProperty(p) ) {
	  var series = data.series[p];
	  series.data.splice(0);
	}
      }

      data.x = 0;

      data.plot.setData(data.flot_data);
      data.plot.setupGrid();			/* remove if grid is fixed */
      data.plot.draw();
    },

    resize: function() {
      var elem  = $(this);

      if ( elem.is(":visible") ) {
	var data  = elem.data(pluginName);
	var xaxis = data.plot.getAxes().xaxis;
	var neww  = Math.round(elem.width()/2);
	var min   = xaxis.options.min;

	data.samples = neww;

	if ( neww < data.x ) {
	  var newmin = data.x - neww;
	  var rm = newmin - min;

	  for(var p in data.series) {
	    if ( data.series.hasOwnProperty(p) ) {
	      var series = data.series[p];

	      series.data.splice(0, rm);
	    }
	  }

	  xaxis.options.max = data.x;
	  xaxis.options.min = newmin;
	} else {
	  xaxis.options.max = xaxis.options.min + data.samples;
	}

	data.plot.resize();
	data.plot.setupGrid();
	data.plot.draw();
      }
    }
  }; // methods

  function suffixFormatter(val, axis) {
    if (val >= 1000000000000)
      return (val / 1000000000000).toFixed(axis.tickDecimals) + " T";
    if (val >= 1000000000)
      return (val / 1000000000).toFixed(axis.tickDecimals) + " G";
    else if (val >= 1000000)
      return (val / 1000000).toFixed(axis.tickDecimals) + " M";
    else if (val >= 1000)
      return (val / 1000).toFixed(axis.tickDecimals) + " K";
    else
      return val.toFixed(axis.tickDecimals) + "  ";
  }

  $("<div id='flot-tooltip'></div>").appendTo("body");

  /**
   * <Class description>
   *
   * @class perfchart
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.perfchart = function(method) {
    if ( methods[method] ) {
      return methods[method]
	.apply(this, Array.prototype.slice.call(arguments, 1));
    } else if ( typeof method === 'object' || !method ) {
      return methods._init.apply(this, arguments);
    } else {
      $.error('Method ' + method + ' does not exist on jQuery.' + pluginName);
    }
  };
}(jQuery));
});
