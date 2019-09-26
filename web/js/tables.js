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
 * Show table with individual tables
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "utils", "modal", "tabulator", "laconic", "form" ],
       function($, config, utils, modal) {

(function($) {
  var pluginName = 'tables';

  /** @lends $.fn.tables */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	utils.busy(elem, true);

	$.ajax({
	  url: config.http.locations.predicate_tables,
	  data: { predicate: options.predicate },
	  success: function(data) {
	    utils.busy(elem, false);
	    elem.tabulator({
	      data:data,
	      layout:"fitDataFill",
	      initialSort:[{column:"answers",dir:"desc"}],
	      columns:columns(),
	      rowClick:function(e, row){
		elem[pluginName]('clicked', row);
	      }
	    });
	  },
	  error: function(jqXHDR) {
	    modal.ajaxError(jqXHDR);
	  }
	});

	elem.data(pluginName, data);	/* store with element */
      });
    },

    clicked: function(row) {
      var variant = row.getData().variant;
      var ws      = $(this).closest(".webstat");

      ws.webstat('show_table', { variant: variant });
    }
  }; // methods

  function columns() {
    var columns = [
     { title:"Variant",
       field:"variant"
     },
     { title:"Answers",
       field:"answers",
       headerTooltip:"# answers in table"
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
     { title:"Variables",
       field:"variables",
       headerTooltip:"# variables in variant"
     },
     { title:"Invalidated",
       field:"invalidated",
       headerTooltip:"Times invalidated"
     },
     { title:"Reeval",
       field:"reevaluated",
       headerTooltip:"Times Reevaluated"
     },
     { title:"Memory",
       field:"space",
       headerTooltip:"Memory used for table"
     },
     { title:"Comp. mem.",
       field:"compiled_space",
       headerTooltip:"Memory used for compiled table"
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
   * @class tables
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.tables = function(method) {
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
