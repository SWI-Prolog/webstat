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

define([ "jquery", "utils", "config", "tabulator", "laconic" ],
       function($, utils, config) {

(function($) {
  var pluginName = 'server_table';

  /** @lends $.fn.server_table */
  var methods = {
    /**
     * Show a server generated table. The server must respond with a
     * JSON object providing `data.data` as an array of row-objects and
     * `data.columns` providing the column description.
     *
     * @param {Object} options
     * @param {Object} [options.query] provides additional query
     * parameter.
     * @param {String} options.handler provides the id of the registered
     * server side handler.
     * @param {Object} [options.table] provides additional options.
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	utils.busy(elem, true);

	function del_props(obj, props) {
	  for(var i=0; i<props.length; i++) {
	    var p = props[i];
	    if ( obj[p] != undefined )
	      delete obj[p];
	  }
	}

	$.get(config.http.locations[options.handler],
	      options.query,
	      function(data) {

		utils.busy(elem, false);

		if ( data.data.length == 0 &&
		     options.onempty ) {
		  options.onempty.call(elem);
		} else {
		  var div = $($.el.div({class: "tabulator-content"}));
		  var opts = $.extend({
		    data:data.data,
		    layout:"fitDataFill",
		    columns:data.columns
		  }, options, data.table);
		  del_props(opts, ['handler', 'query', 'onempty']);

		  elem.empty().append(div);
		  div.tabulator(opts);
		}
	      });

	elem.data(pluginName, data);	/* store with element */
      });
    },

    setFilter: function(field, cmp, val) {
      var elem = $(this);

      elem.find(".tabulator-content").tabulator('setFilter', field, cmp, val);
    }
  }; // methods

  /**
   * <Class description>
   *
   * @class server_table
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.server_table = function(method) {
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
