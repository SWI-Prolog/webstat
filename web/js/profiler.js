/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
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
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "utils", "modal", "form",
	 "tabulator", "laconic", "server_table" ],
       function($, config, utils, modal, form, tabulator) {

(function($) {
  var pluginName = 'profiler';

  /** @lends $.fn.profiler */
  var methods = {
    /**
     * @param {Object} [options]
     * @param {String} [options.predicate] Show the call graph around
     * this predicate
     */
    _init: function(options) {
      options = options||{};

      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	elem.addClass("profiler");
	elem.append($.el.div({class:"form-inline prof_controller"}),
		    $.el.div({class:"prof_content"},
			     $.el.div({class:"prof_predicates"}),
			     $.el.div({class:"prof_graph graphviz-sizer"})));
	elem[pluginName]('controller');
	elem[pluginName]('show_predicates');
	if ( options.predicate )
	  elem[pluginName]('graph', options.predicate);

	elem.data(pluginName, data);	/* store with element */
      });
    },

    controller: function() {
      var elem = $(this);
      var ctrl = elem.find(".prof_controller");
      var br;

      tabulator.add_filter(ctrl, function(val) {
	elem.find(".prof_predicates")
            .server_table('setFilter', "predicate", "like", val);
      });

      ctrl.append(br=$($.el.div({class:"btn-group"})));
      br.append($("<span class='menu-space'>&nbsp</span>"),
		form.widgets.glyphIconButton("erase", {
		  action:'reset', title:"Clear recorded profile data"}),
		$("<span class='menu-space'>&nbsp</span>"),
		form.widgets.glyphIconButton("film", {
		  action:'record', title:"Record profile data"}),
		form.widgets.glyphIconButton("pause", {
		  action:'pause', title:"Pause recording profile data"}),
		$("<span class='menu-space'>&nbsp</span>"),
		form.widgets.glyphIconButton("eye-open", {
		  action:'show', title:"Show profile data"}));

      br.on("click", ".btn", function(ev) {
	var action = $(ev.target).closest(".btn").data('action');

	if ( action == 'show' ) {
	  elem[pluginName]('show_predicates');
	} else {
	  $.get(config.http.locations.prof_control + action,
		function(data) {
		  if ( typeof(data) == 'object' ) {
		    if ( data.new == 'cputime' ) {
		      elem.find(".glyphicon-film").addClass("recording");
		    } else if ( data.new == false ) {
		      elem.find(".glyphicon-film").removeClass("recording");
		      if ( data.clear ) {
			elem.find(".prof_predicates").empty();
			elem.find(".prof_graph").empty();
			elem[pluginName]('help');
		      }
		    }
		  }
		});
	}
      });

    },

    show_predicates: function() {
      var elem = $(this);
      var opts = { query: {
			  },
		   handler:"prof_predicates",
		   rowClick:function(e, row){
		     elem[pluginName]('clicked', row);
		   },
		   onempty: function() {
		     elem[pluginName]('help');
		   }
		 };

      elem.find(".prof_predicates").server_table(opts);
    },

    help: function() {
      var elem = $(this);

      $.get(config.http.locations.webstat_help + "/profiler.html",
	    function(html) {
	      var div;
	      elem.find(".prof_predicates")
		  .empty()
		  .append(div=$($.el.div({class:"prof_help"})));
	      div.html(html);
	    });
    },

    clicked: function(row) {
      var elem = $(this);
      var pred = row.getData().predicate;	/* predicate indicator */

      elem[pluginName]('graph', pred);
    },

    graph: function(pred) {
      var elem = $(this);

      $.get(config.http.locations.prof_graph,
	    { focus: pred },
	    function(html) {
	      var div = elem.find(".prof_graph");
	      var hld = $($.el.div({class:"graph-holder",
				    style:"width:100px; height:100px;"}));

	      div.empty().append(hld);
	      hld.html(html);
	      utils.evalScripts(hld);
	      finish(div.find("svg"));
	    });
    }
  }; // methods

  function finish(svg) {
    var focus = svg.find("ellipse[stroke-width=2]");

    function pred(ev) {
      return $(ev.target).closest("a").attr("xlink:href");
    }

    svg.on("click", "text", function(ev) {
      modal.predicate_details({predicate: pred(ev)});
      return false;
    });
  }

  /**
   * <Class description>
   *
   * @class profiler
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.profiler = function(method) {
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
