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
 * This file deals with tabbed panes.  It implements dynamic tabs on top
 * if Bootstrap.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "laconic" ],
       function($) {
var tabbed;

(function($) {
  var pluginName = 'tabbed';
  var tabid = 0;

  /** @lends $.fn.tabbed */
  var methods = {
    /**
     * Turn the current element into a Bootstrap tabbed pane. All
     * children of the current element are changed into tabs.  The
     * child can control the mapping using:
     *
     *   - `data-label = "Label"`
     *   - `data-close = "disabled"`
     */
    _init: function(options) {
      options = options||{};

      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	data.newTab   = options.newTab;
	elem.data(pluginName, data);	/* store with element */

	elem.addClass("tabbed");
	elem.tabbed('makeTabbed');
      });
    },

    /**
     * Turn the pane into a tabbed pane
     */
    makeTabbed: function() {
      var data = this.data(pluginName);
      var children = this.children();
      var ul = $.el.ul({ class:"nav nav-tabs",
			 role:"tablist"
		       });
      var contents = $.el.div({class:"tab-content"});

      this.prepend(contents);
      this.prepend(ul);

      $(ul).on("click", "span.xclose", function(ev) {
	var id = $(ev.target).parent().attr("data-id");
	$(ev.target).parents(".tabbed").first().tabbed('removeTab', id);
	ev.preventDefault();
      });
      $(ul).on("click", "a", function(ev) {
	$(ev.target).closest("a").tab('show');
	ev.preventDefault();
      });

			/* Turn children into tabs */
      for(var i=0; i<children.length; i++) {
	var child = $(children[i]);
	var id = genId();
	var label = child.attr("data-label") || "Unknown";
	var close = child.attr("data-close") != "disabled";
	var active = (i == children.length-1);	/* activate last */

	var li = this.tabbed('tabLabel', id, label, close);
	if ( active )
	  $(li).addClass("active");
	$(ul).append(li);
	$(contents).append(wrapInTab($(children[i]), id, active));
      }

      if ( data.newTab ) { /* Create and handle "+" button */
	var create = $.el.a({ class: "tab-new compact",
			      title: "Open a new tab"
			    },
			    glyphicon("plus"));
	$(ul).append($.el.li({ class: "tab-new", role:"presentation" }, create));
	$(create).on("click", function(ev) {
	  var tabbed = $(ev.target).parents(".tabbed").first();

	  tabbed.tabbed('newTab');
	  ev.preventDefault();
	  return false;
	});
      }

			/* Handle tab-switching */
      $(ul).on("shown.bs.tab", "a", function(ev) {
	var newContentID = $(ev.target).data("id");
	$("#"+newContentID+" .swish-event-receiver").trigger("activate-tab");
      });

      if ( data.newTab ) {
	if ( this.tabbed('navContent').children().length == 0 ) {
	  this.tabbed('newTab');
	}
      }
    },

    /**
     * Add an empty new tab from the "+" button.  This calls
     * options.newTab() to return a DOM element for the new
     * tab.
     * @param {HTMLElement} [content] Content for the new tab
     * If omitted, it calls `options.newTab` or uses the method
     * `tabSelect`.
     * @return {jQuery} object representing the created tab
     */
    newTab: function(dom, active) {
      var data = this.data(pluginName);

      if ( dom == undefined ) {
	if ( data.newTab ) {
	  dom = data.newTab();
	}
      }

      if ( active == undefined )
	active = true;

      return this.tabbed('addTab', dom, {active:active,close:true});
    },

    /**
     * Add a new tab using content
     * @param {Object} content is the DOM node to use as content for the
     * tab.
     * @param {Object} options
     * @param {Boolean} [options.active] if `true`, make the new tab
     * active
     * @param {Boolean} [options.close] if `true`, allow closing the new
     * tab.
     * @param {String} [options.label] is the label of the tab
     * @param {String} [options.type] type of the tab (defines icon)
     * @return {jQuery} the created tab element
     */
    addTab: function(content, options) {
      var ul    = this.tabbed('navTabs');
      var id    = genId();
      var label = options.label||"New tab";
      var type  = options.type||"swipl";
      var tab   = wrapInTab(content, id, options.active);

      this.tabbed('navContent').append(tab);

      var li  = this.tabbed('tabLabel', id, label, close, type);

      var create = ul.find("a.tab-new");
      if ( create.length == 1 )
	$(li).insertBefore(create.first().parent());
      else
	ul.append(li);

      if ( options.active )
	$(li).find("a").first().tab('show');

      return tab;
    },

    /**
     * Remove tab with given Id. If the tab is the active tab, make the
     * previous tab active, or if there is no previous, the next. If the
     * tabbed environment becomes empty, add a virgin tab.
     *
     * @param {String} id is the id of the tab to destroy
     */
    removeTab: function(id) {
      var data = this.data(pluginName);
      var li  = this.tabbed('navTabs').find("a[data-id='"+id+"']").parent();
      var tab = $("#"+id);
      var new_active;

      if ( tab.is(":visible") )
	new_active = li.prev() || li.next();
      li.remove();
      tab.remove();
      if ( new_active && new_active.length > 0 ) {
	new_active.find("a").first().tab('show');
      } else if ( this.tabbed('navContent').children().length == 0 &&
		  data.newTab ) {
	this.tabbed('newTab');
      }
    },

    /**
     * Show indicated tab.
     * @param {String} id is the id of the tab to show.
     */
    show: function(id) {
      var a = this.tabbed('navTab', id);
      if ( a ) {
	a.tab('show');
      }

      $(".storage").storage('chat_status', true);
    },

    /**
     * Move the argument tab or tab id to the right of all
     * tabs.
     */
    move_right: function(tab) {
      var id;
      var ul = this.find(">ul");

      if ( typeof(tab) == "string" )
	id = tab;
      else
	id = tab.attr('id');

      ul.find("a[data-id="+id+"]")
        .closest("li")
        .insertBefore(ul.children().last());
    },

    /**
     * Create a label (`li`) for a new tab.
     * @param {String} id is the identifier of the new tab
     * @param {String} label is the textual label of the new tab
     * @param {Boolean} close determines whether or nor a close button
     * is added to the tab.
     * @param {String} [type="pl"] indicates the type of the tab. This
     * is used for associating an icon with the tab.
     */
    tabLabel: function(id, label, close, type) {
      var close_button;
      var chat;

      if ( close )
      { close_button = glyphicon("remove", "xclose");
	$(close_button).attr("title", "Close tab");
      }
      type = type||"pl";

      var a1 = $.el.a({class:"compact", href:"#"+id, "data-id":id},
		      $.el.span({class:"tab-icon type-icon "+type}),
		      $.el.span({class:"tab-title"}, label),
		      close_button);
      var li = $.el.li({role:"presentation"}, a1);

      return li;
    },

    /**
     * Calling obj.tabbed('anchor') finds the <a> element
     * representing the tab label from the node obj that appears
     * somewhere on the tab
     */
    anchor: function() {
      var tab    = this.closest(".tab-pane");

      if ( tab.length == 0 ) {
	return undefined;		/* e.g., fullscreen mode */
      }

      var tabbed = tab.closest(".tabbed");
      var id     = tab.attr("id");
      var ul	 = tabbed.tabbed('navTabs');
      var a      = ul.find("a[data-id="+id+"]");

      return a;
    },

    /**
     * This method is typically _not_ called on the tab, but on some
     * inner element of the tab.  It changes the title of the tab.
     * @param {String} title is the new title for the tab.
     * @param {String} [type="pl"] is the new type for the tab.
     */
    title: function(title, type) {
      var a = this.tabbed('anchor');

      if ( a ) {
	a.find(".tab-title").text(title);
	if ( type ) {
	  var icon = a.find(".tab-icon");
	  icon.removeClass();
	  icon.addClass("tab-icon type-icon "+type);
	}
      }

      return this;
    },

    /**
     * Get the UL list that represents the nav tabs
     */
    navTabs: function() {
      return this.find("ul.nav-tabs").first();
    },

    navTab: function(id) {
      var a = this.find("ul.nav-tabs").first().find("a[data-id='"+id+"']");
      if ( a.length > 0 )
	return a;
    },

    navContent: function() {
      return this.find("div.tab-content").first();
    }
  }; // methods

  /**
   * Wrap a content element in a Bootstrap tab content.
   * @param {Object} dom is the object that must be wrapped
   * @param {String} id is the identifier to give to the new content
   * @param {Boolean} active sets the tab to active if `true`
   * @return {jQuery} `div` object of class `tab-pane` and the
   * passed `id`.
   */
  function wrapInTab(dom, id, active) {
    $(dom).wrap('<div role="tabpanel" class="tab-pane" id="'+id+'"></div>');
    var wrapped = $(dom).parent();

    if ( active )
      wrapped.addClass("active");

    return wrapped;
  }

  function glyphicon(glyph, className) {
    var span = $.el.span({class:"glyphicon glyphicon-"+glyph});

    if ( className )
      $(span).addClass(className);

    return span;
  }

  function genId()
  { return "tabbed-tab-"+tabid++;
  }

  /**
   * <Class description>
   *
   * @class tabbed
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.tabbed = function(method) {
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

  return tabbed;
});
