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
 * Deal with the webstat websocket connection to accept push
 * events.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "laconic" ],
       function($, config) {

var MIN_RECONNECT_DELAY =  10000;
var MAX_RECONNECT_DELAY = 300000;

(function($) {
  var pluginName = 'ws_push';
  var reconnect_delay = MIN_RECONNECT_DELAY;
  var last_open = null;

  /** @lends $.fn.ws_push */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */
	elem.data(pluginName, data);	/* store with element */

	if ( config.http.locations.webstat_push ) {
	  elem[pluginName]('connect');
	}
      });
    },

    connect: function() {
      var elem = this;
      var data = this.data(pluginName);
      var url  = window.location.host + config.http.locations.webstat_push;
      var ws   = window.location.protocol.replace("http", "ws");

      if ( data.connection && data.connection.readyState != 3 )
	return this;			/* already connecting, open or closing */

      try {
	data.connection = new WebSocket(ws + "//" + url,
					['v1.webstat.swi-prolog.org']);
      } catch(err) {
	console.log(err);
	return;
      }

      data.connection.onerror = function(error) {
	console.log(error);
      };
      data.connection.onclose = function(ev) {
	console.log("Closing websocket", ev);
	if ( last_open == null ) {
	  reconnect_delay *= 2;
	  if ( reconnect_delay > MAX_RECONNECT_DELAY )
	    reconnect_delay = MAX_RECONNECT_DELAY;
	} else {
	  if ( getTime() - last_open > 300000 )
	  { reconnect_delay = MIN_RECONNECT_DELAY;
	  } else
	  { reconnect_delay *= 2;
	    if ( reconnect_delay > MAX_RECONNECT_DELAY )
	      reconnect_delay = MAX_RECONNECT_DELAY;
	  }
	}
	setTimeout(function() {
	  elem[pluginName]('connect');
	}, reconnect_delay);
      };
      data.connection.onmessage = function(e) {
	var msg = JSON.parse(e.data);
	msg.origin = e.origin;
	if ( msg.type )
	  elem[pluginName](msg.type, msg);
	else
	  console.log(e);
      };
      data.connection.onopen = function() {
	console.log("push connection is open");
      };
    },

    alert: function(msg) {
      console.log(msg);
    }
  }; // methods

  function getTime() {
    var d = new Date();
    return d.getTime();
  }

  /**
   * <Class description>
   *
   * @class ws_push
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.ws_push = function(method) {
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
