/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2019, VU University Amsterdam
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

define([ "jquery", "tabulator-tables", "laconic" ],
       function($, Tabulator) {

(function($) {
  var pluginName = 'tabulator';

  /** @lends $.fn.tabulator */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	elem.data(pluginName, data);	/* store with element */

	elem.addClass("reactive-size swish-event-receiver");
	elem.on('reactive-resize', function() {
	  if ( elem.is(":visible") )
	    elem[pluginName]('redraw');
	});
	elem.on('activate-tab', function() {
	  elem[pluginName]('redraw');
	});

	elem.css("height", elem.closest(".tab-pane").height()+"px");
	data.table = new Tabulator(elem[0], options);
	data.table.redraw();

	elem.data(pluginName, data);	/* store with element */
      });
    },

    redraw: function() {
      var elem = $(this);
      var data = elem.data(pluginName);

      elem.css("height", elem.closest(".tab-pane").height()+"px");
      if ( data.table )
	data.table.redraw();
    },

    setFilter: function(field, cmp, val) {
      var data = $(this).data(pluginName);

      data.table.setFilter(field, cmp, val);
    }
  }; // methods

  /**
   * <Class description>
   *
   * @class tabulator
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.tabulator = function(method) {
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

return {
  add_filter: function(dom, onchange) {
    var input;

    dom.append($.el.div({class:"input-group"},
			$.el.span({class:"input-group-addon"},
				  $.el.i({class:"glyphicon glyphicon-filter"})),
			input =
			$.el.input({type:"text", class:"form-control",
			            name:"filter", placeholder:"Filter"})));

    $(input).on('input', function() {
      var txt = $(input).val();

      onchange.call(dom, txt);
    });
  }
};
});
