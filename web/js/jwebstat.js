/*  Part of webstat

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
 * <Description of the File>
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@cwi.nl
 * @requires jquery
 */

define([ "jquery",
	 "modal",
	 "config",
	 "laconic",
	 "navbar",
	 "bootstrap",
	 "tabbed",
	 "push",

	 "tabled_preds",
	 "tables",
	 "IDG",
	 "server_table",
	 "table",
	 "perfchart",
	 "profiler",
	 "debugmsg"
       ],
       function($, modal, config) {

(function($) {
  var pluginName = 'webstat';

  var defaults = {
    menu: {
      "Performance":
      { "Resource charts": function() {
	  $("body").webstat('show_perfchart');
	},
	"Execution profiler": function() {
	  $("body").webstat('show_profiler');
	},
      },
      "Debug":
      { "Debug messages": function() {
	  $("body").webstat('show_debug_messages');
	}
      },
      "Tabling":
      { "Tabled predicates": function() {
	  $("body").webstat('show_tabled_predicates');
	},
	"Incremental Dependency Graph": function() {
	  $("body").webstat('show_idg');
	}
      }
    }
  }; // defaults

  /** @lends $.fn.webstat */
  var methods = {
    _init: function(options) {
      options = options||{};
      this.addClass("webstat");

      setupModal();
      setupResize();

      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	$("#navbar").navbar(defaults.menu);
	$("#content").tabbed();

	elem.data(pluginName, data);	/* store with element */
	elem.ws_push();
      });
    },

    /** Add a new tab
     *
     * @return the div element that is the content of the tab
     */
    tab: function(options) {
      var dom = $($.el.div());
      var opts;

      if ( typeof(options) == "object" )
	opts = options;
      else if ( typeof(options) == "string" )
	opts = {label:options};
      else if ( !options )
	opts = {};

      if ( opts.active == undefined ) opts.active = true;
      if ( opts.close  == undefined ) opts.close  = true;

      if ( opts.reuse ) {
	var ontab = $("#content").find(opts.reuse);
	if ( ontab.length > 0 ) {
	  ontab.tabbed('anchor').tab('show');
	  return ontab;
	}
      }

      $("#content").tabbed('addTab', dom, opts);
      if ( opts.page ) {
	dom.closest(".tab-content").addClass("tab-page");
	dom.closest(".tab-pane").addClass("tab-page");
      }
      return dom;
    },

		 /*******************************
		 *    CREATE UI COMPONENTS	*
		 *******************************/

    /**
     * Show debug messages
     */
    show_debug_messages: function(options) {
      this.webstat('tab', {
        label: "Debug messages",
	page: true
      }).ws_debug(options);
    },

    /**
     * Show tabled predicates
     */
    show_tabled_predicates: function(options) {
      this.webstat('tab', {
        label: "Tabled predicates",
	page: true
      }).tabled_preds(options);
    },

    /**
     * Show tables for a predicate.
     */
    show_tables: function(options) {
      this.webstat('tab', {
        label: "Tables for "+options.predicate,
	page: true
      }).tables(options);
    },

    /**
     * Show IDG from a predicate or all.
     */
    show_idg: function(options) {
      options = options||{};
      var label = (options.predicate ? "IDG for " + options.predicate
				     : "IDG");

      this.webstat('tab', label).IDG(options);
    },

    /**
     * Show a sampled resource usage charts
     */
    show_perfchart: function(options) {
      this.webstat('tab', "Resource usage").perfchart(options);
    },

    /**
     * Show listing of (notably dynamic) facts
     */
    listing: function(options) {
      var opts = { query:  { pi:options.predicate
			   },
		   handler:"fact_table"
		 };

      this.webstat('tab', {
        label: "Clauses for " + options.predicate,
	page: true
      }).server_table(opts);
    },

    /**
     * Show an individual table
     */
    show_table: function(options) {
      this.webstat('tab', {
        label: "Table for " + options.variant,
	page: true
      }).table(options);
    },

    /**
     * Show profiler
     */
    show_profiler: function(options) {
      this.webstat('tab', {
        label: "Profiler",
	page: true,
	reuse: ".profiler"
      }).profiler(options);
    }
  }; // methods

  /**
   * Setup modal actions.  Subsequently, modal dialogue windows
   * are opened by using the trigger `help`.
   * @example $("body").swish('action', 'help', {file:"about.html"});
   */
  function setupModal() {
    if ( $("#modal").length == 0 ) {
      $("body").append($.el.div({id:"modal"}));
      $("#modal").swishModal();
    }
  }

  function setupResize() {
    $(window).resize(function() {
      $(".reactive-size").trigger('reactive-resize');
    });
  }

  /**
   * <Class description>
   *
   * @class webstat
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.webstat = function(method) {
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
