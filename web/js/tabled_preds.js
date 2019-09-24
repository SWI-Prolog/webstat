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
 * <Description of the File>
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "utils", "modal", "form",
	 "tabulator", "laconic", "form" ],
       function($, config, utils, modal, form) {

(function($) {
  var pluginName = 'tabled_preds';

  /** @lends $.fn.tabled_preds */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	elem.data(pluginName, data);	/* store with element */
	elem.addClass("tabled-predicates listen-close-event");
	elem.append($.el.div({class:"form-inline tpred_controller"}),
		    $.el.div({class:"tpred_content"}));
	elem[pluginName]('controller');
	elem[pluginName]('load');

	elem.on('tab-close', function() {
	  if ( data.timer ) {
	    clearInterval(data.timer);
	    delete(data.timer);
	  }
	});
      });
    },

    controller: function() {
      var elem = $(this);
      var data = elem.data(pluginName);
      var input, br;

      /* Filter element */
      elem.find(".tpred_controller")
          .append($.el.div({class:"input-group"},
			   $.el.span({class:"input-group-addon"},
				     $.el.i({class:"glyphicon glyphicon-filter"})),
			   input =
			   $.el.input({type:"text", class:"form-control",
				       name:"filter", placeholder:"Filter"})));
      $(input).on('input', function() {
	var txt = $(input).val();
	elem.find(".tpred_content")
	    .tabulator('setFilter', "variant", "like", txt);
      });

      /* buttons */
      elem.find(".tpred_controller")
          .append(br=$($.el.div({class:"btn-group"})));
      br.append($("<span class='menu-space'>&nbsp</span>"),
		form.widgets.glyphIconButton("refresh", {
	          action:'refresh', title:"Refresh"}),
		form.widgets.glyphIconButton("repeat", {
	          action:'repeat', title:"Refresh repeatedly"}));
      br.on("click", ".btn", function(ev) {
	var action = $(ev.target).closest(".btn").data('action');

	if ( action == 'refresh' ) {
	  elem[pluginName]('load');
	} else if ( action == 'repeat' ) {
	  var btn = br.find(".glyphicon-repeat");

	  if ( data.timer ) {
	    clearInterval(data.timer);
	    delete data.timer;
	    btn.removeClass("repeat-running");
	    modal.feedback({html: "Stopped refresh"});
	  } else {
	    data.timer = setInterval(function() {
	      elem[pluginName]('load');
	    }, 10000);
	    btn.addClass("repeat-running");
	    modal.feedback({html: "Refreshing every 10 seconds"});
	  }
	}
      });
    },

    load: function() {
      var elem = $(this);
      var data = elem.data(pluginName);
      var start = Date.now();

      utils.busy(elem, true);

      $.get(config.http.locations.tabled_predicates,
	    function(sdata) {
	      var ifilter;

	      utils.busy(elem, false);
	      data.poll_time = Date.now()-start;

	      var opts = {
		data:sdata,
		layout:"fitDataFill",
		initialSort:[{column:"tables",dir:"desc"}],
		columns:columns(),
		rowClick:function(e, row){
		  elem[pluginName]('clicked', row);
		}
	      };

	      ifilter = elem.find(".tpred_controller input").val();
	      if ( ifilter != "" )
		opts.initialFilter = [ {field:"variant", type:"like", value: ifilter} ];

	      elem.find(".tpred_content").empty().tabulator(opts);
	    });
    },

    clicked: function(row) {
      var pred = row.getData().variant;	/* predicate indicator */

      modal.predicate_details({ predicate: pred });
    }
  }; // methods

  function columns() {
    var columns = [
     { title:"Predicate",
       field:"variant"
     },
     { title:"D",
       field:"dynamic",
       sorter:"boolean",
       headerTooltip:"Dynamic"
     },
     { title:"I",
       field:"incremental",
       sorter:"boolean",
       headerTooltip:"Incremental"
     },
     { title:"G",
       field:"shared",
       sorter:"boolean",
       headerTooltip:"Shared"
     },
     { title:"S",
       field:"subsumptive",
       sorter:"boolean",
       headerTooltip:"Subsumptive"
     },
     { title:"Tables",
       field:"tables",
       headerTooltip:"# tabled variants"
     },
     { title:"Answers",
       field:"answers",
       headerTooltip:"# answers summed over variants"
     },
     { title:"Clauses",
       field:"clauses",
       headerTooltip:"# clauses for incremental dynamic predicates"
     },
     { title:"Compl. calls",
       field:"complete_call",
       headerTooltip:"# calls to a complete table"
     },
     { title:"Dupl. ratio",
       field:"duplicate_ratio",
       headerTooltip:"Generated duplicate answers",
       formatter:"money",
       formatterParams:{precision:2}
     },
     { title:"Invalid",
       field:"invalid",
       headerTooltip:"# invalid tables"
     },
     { title:"Invalidated",
       field:"invalidated",
       headerTooltip:"# invalidated tables"
     },
     { title:"Reeval",
       field:"reevaluated",
       headerTooltip:"# reevaluated tables"
     },
     { title:"Memory",
       field:"space",
       headerTooltip:"Memory used for answer tables"
     },
     { title:"Comp. mem.",
       field:"compiled_space",
       headerTooltip:"Memory used for compiled answer tables"
     },
     { title:"Space ratio",
       field:"space_ratio",
       headerTooltip:"Values / trie nodes",
       formatter:"money",
       formatterParams:{precision:2}
     }
    ];

    for(i=0; i<columns.length; i++) {
      var col = columns[i];

      if ( col.field == "variant" )
	continue;

      if ( col.sorter == "boolean" ) {
	col.formatter = "tickCross";
	col.formatterParams = {crossElement:false};
	col.align = "center";
      }

      if ( !col.sorter ) {
	headerSortStartingDir = "desc";
	col.sorter = "number";
	col.align = "right";
      }

      if ( !col.formatter ) {
	col.formatter = "money";
	col.formatterParams = {thousand:",", precision:false};
      }
    }

    return columns;
  }

  /**
   * <Class description>
   *
   * @class tabled_preds
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.tabled_preds = function(method) {
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
