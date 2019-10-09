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
 * Handle debug/3 messages
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "modal", "tabulator", "form",
	 "laconic"
       ],
       function($, config, modal, tabulator, form) {

(function($) {
  var pluginName = 'ws_debug';

  var topic_columns = [
	  { title:"Topic",
	    field:"topic",
	    width:180
	  },
	  { title:"Active",
	    field:"active",
	    align:"center",
	    sorter:"boolean",
	    formatter:"tickCross",
	    cellClick: function(ev, cell) {
	      var row = cell.getRow().getData();
	      var elem = $(ev.target).closest(".ws_debug");
	      elem[pluginName]('activate_topic', row.topic, !row.active, row.id);
	    }
	  }
      ];
  var msg_columns = [
	  { title:"No",
	    field:"id",
	    sorter:"number",
	    align:"right",
	    formatter:"money",
	    formatterParams:{thousand:",", precision:false},
	    width:60
	  },
	  { title:"Thread",
	    field:"thread",
	    width:90
	  },
	  { title:"Topic",
	    field:"topic",
	    width:100
	  },
	  { title:"Message",
	    field:"message",
	    widthGrow:100
	  }
      ];

  /** @lends $.fn.ws_debug */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	data.messages = [];
	elem.data(pluginName, data);	/* store with element */

	elem.addClass("ws_debug");
	elem.append($.el.div({class:"ws_debug_content"},
			     $.el.div({class:"ws_debug_topics"}),
			     $.el.div({class:"ws_debug_messages"})));
	elem[pluginName]('topics');
	elem[pluginName]('messages');
      });
    },

    topics: function() {
      var elem  = $(this);
      var data  = elem.data(pluginName);
      var telem = elem.find(".ws_debug_topics");

      telem.append($.el.div({class:"form-inline ws_debug_controller"}),
		   $.el.div({class:"ws_debug_table"}));

      var ctrl = telem.find(".ws_debug_controller");
      tabulator.add_filter(ctrl, function(val) {
	telem.server_table('setFilter', "topic", "like", val);
      });

      $.ajax({
        url: config.http.locations.debug_topics,
	success: function(reply) {
	  var div = $($.el.div({class: "tabulator-content"}));
	  var opts = {
	    data: reply.topics,
	    layout:"fitData",
	    columns:topic_columns,
	    initialSort: [
		{ column:"topic", dir:"asc" },
		{ column:"active", dir:"desc" }
	    ]
	  };

	  telem.find(".ws_debug_table").empty().append(div);
	  div.tabulator(opts);
	  data.topic_table = div.tabulator('table');
	  elem[pluginName]('active');
	},
	error: function(jqXHDR) {
	  modal.ajaxError(jqXHDR);
	}
      });
    },

    activate_topic: function(topic, val, id) {
      var elem = $(this);
      var data = elem.data(pluginName);

      $.ajax({
        url: config.http.locations.debug_topic,
	data: {
	  topic: topic,
	  active: val
	},
	success: function(reply) {
	  reply.id = id;
	  data.topic_table.updateRow(id, reply);
	},
	error: function(jqXHDR) {
	  modal.ajaxError(jqXHDR);
	}
      });
    },

    messages: function() {
      var elem = $(this);
      var data = elem.data(pluginName);
      var msgelem = elem.find(".ws_debug_messages");
      var div = $($.el.div({class: "tabulator-content"}));

      msgelem.append($.el.div({class:"form-inline ws_debug_controller"}),
		     $.el.div({class:"ws_debug_table"}));

      var ctrl = msgelem.find(".ws_debug_controller");
      ctrl.append(form.widgets.glyphIconButton("erase", {
		    action:'clear', title:"Clear messages"}),
		  form.widgets.separator());
      tabulator.add_filter(ctrl, function(val) {
	msgelem.server_table('setFilter', "message", "like", val);
      });

      ctrl.on("click", function(ev) {
	var action = $(ev.target).closest(".btn").data('action');
	if ( typeof(action) == "string" )
	  elem[pluginName](action);
      });

      var opts = {
	    //reactiveData:true,
	    data: data.messages,
	    layout:"fitColumns",
	    placeholder:"No debug messages",
	    columns:msg_columns,
	    voffset: 34			/* Space for controller */
	  };

      msgelem.find(".ws_debug_table").empty().append(div);
      div.tabulator(opts);
      data.msgtable = div.tabulator('table');
    },

    clear: function() {
      var elem = $(this);
      var data = elem.data(pluginName);

      data.messages = [];
      data.msgtable.setData(data.messages);

      $.ajax({
        url: config.http.locations.debug_reset,
	success: function(data) {
	},
	error: function(jqXHDR) {
	  modal.ajaxError(jqXHDR);
	}
      });
    },

    active: function(val) {
      if ( val == undefined )
	val = $(".ws_debug").length > 0;

      $.ajax({
        url: config.http.locations.debug_forward,
	data: { val:val },
	success: function(data) {
	},
	error: function(jqXHDR) {
	  modal.ajaxError(jqXHDR);
	}
      });
    },

    /**
     * @param {Object} msg
     * @param {String} msg.message is the textual message
     * @param {String} msg.topic is the topic it was sent to
     * @param {Float}  msg.time is the time stamp of the message
     * @param {String|Integer} is the thread id of the message
     */
    add_message: function(msg) {
      var elem = $(this);
      var data = elem.data(pluginName);

      if ( data ) {
	data.msgtable.addRow(msg);
	if ( data.add_timer )
	  clearTimeout(data.add_timer);
	data.add_timer = setTimeout(function() {
	  data.add_timer = null;
	  elem[pluginName]('scroll_to_bottom');
	}, 50);
      }
    },

    scroll_to_bottom: function() {
      var data = $(this).data(pluginName);
      var rows = data.msgtable.getRows();

      if ( rows.length > 0 ) {
	var last = rows[rows.length-1].getData();
	data.msgtable.scrollToRow(last.id, "top", true);
      }
    }
  }; // methods


  /**
   * <Class description>
   *
   * @class ws_debug
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.ws_debug = function(method) {
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
