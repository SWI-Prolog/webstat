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

define([ "jquery", "config", "flot", "laconic" ],
       function($, config, plot) {

(function($) {
  var pluginName = 'perfchart';

  var plotopts = {
    xaxis: {
      min: 0,
      max: 1000
    },
    yaxis: {
      min: 1,
      max: 10000000000,
      ticks: [ 1,10,100,1000,10000,100000,
	       1000000,10000000,100000000,1000000000,
	       10000000000
	     ],
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

	elem.data(pluginName, data);	/* store with element */
	elem.addClass("perfchart");
	elem.append($.el.div({class:"flot"}));
	elem[pluginName]('tooltip');

	$.get(config.http.locations.perf_series,
	      function(data) {
		elem[pluginName]('series', data);
	      });
      })
    },

    /**
     * Initialize the data series and flot instance
     */
    series: function(options) {
      var elem = $(this);
      var data = elem.data(pluginName);

      data.rate      = Math.round((options.rate||1)*1000);
      data.series    = options.series;
      data.x	     = 0;
      data.flot_data = [];

      for(var p in options.series) {
	if ( options.series.hasOwnProperty(p) ) {
	  var series = options.series[p];
	  var i = data.flot_data.length;

	  series.index = i;
	  series.data  = [];

	  data.flot_data.push(series);
	}
      }

      data.plot = $.plot(elem.find(".flot"), data.flot_data, plotopts);
      data.plot.setupGrid();
      elem[pluginName]('update');

      setInterval(function() {
	elem[pluginName]('update');
      }, data.rate);
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

	if ( data.flot_data[0] && data.flot_data[0].data &&
	     data.flot_data[0].data[x] ) {
	  var global = data.flot_data[0].data[x][1];

	  $("#flot-tooltip").html("Global = "+global)
	      .css({top: pos.pageY+5, left: pos.pageX+5})
	      .show();
	} else {
	  $("#flot-tooltip").hide();
	}
      });
    },

    update: function() {
      var elem = $(this);

      $.get(config.http.locations.perf_sample,
	    function(data) {
	      elem[pluginName]('add_sample', data);
	    });
    },

    /**
     * Add an individual sample
     */
    add_sample: function(options) {
      var data   = $(this).data(pluginName);
      var sample = options.sample||{};
      var x      = data.x++;

      for(var p in sample) {
	if ( sample.hasOwnProperty(p) ) {
	  data.flot_data[data.series[p].index].data[x] = [x, sample[p]];
	}
      }

      data.plot.setData(data.flot_data);
      data.plot.setupGrid();			/* remove if grid is fixed */
      data.plot.draw();
    }
  }; // methods

  $("<div id='flot-tooltip'></div>").css({
    position: "absolute",
    display: "none",
    border: "1px solid #fdd",
    padding: "2px",
    "background-color": "#fee",
    opacity: 0.80
  }).appendTo("body");


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
