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

define([ "jquery", "laconic" ],
       function() {

(function($) {
  var pluginName = 'form';

  /** @lends $.fn.form */
  var methods = {
    _init: function(options) {
    },

    button_row: function(actions) {
      var div = $.el.div({class:"btn-group text-center"});

      $(this).append(div);
      for(var p in actions) {
	if ( actions.hasOwnProperty(p) ) {
	  var btn = $.el.button({ class:"btn btn-primary",
				  "data-dismiss":"modal"
				}, p);
	  $(div).append(btn);
	  $(btn).on("click", actions[p]);
	}
      }
    }
  }; // methods

  /**
   * <Class description>
   *
   * @class form
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.form = function(method) {
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

  var form = {
    widgets: {
      glyphIcon: function(glyph) {
	return $.el.span({class:"glyphicon glyphicon-"+glyph});
      },

      typeIcon: function(type) {
	return $.el.span({class:"dropdown-icon type-icon "+type});
      },

      glyphIconButton: function(glyph, options) {
	var attrs = {class:"btn", type:"button"};

	if ( options.action ) attrs['data-action'] = options.action;
	if ( options.title )  attrs.title          = options.title;
	if ( options.class )  attrs.class	  += " "+options.class;

	return $.el.button(attrs, form.widgets.glyphIcon(glyph));
      },

      separator: function() {
	return $("<span class='menu-space'>&nbsp</span>");
      },

      checkbox: function(name, options) {
	var opts = {type:"checkbox", name:name};
	var lopts = {};
	var dopts = {class:"checkbox"};

	if ( options.checked )
	  opts.checked="checked";
	if ( options.color )
	  lopts.style="color:"+options.color;
	if ( options.title )
	  dopts.title = options.title;

	return $.el.div(dopts,
			$.el.label(lopts,
				   $.el.input(opts),
				   options.label||name));
      },

      /**
       * Turn an icon into a dropdown button.
       * @param {Object} options
       * @param {Any}	 options.client is the `this` for the menu
       *		 functions.
       * @param {String} [options.divClass] additional class for the
       * returned `div` element
       * @param {String} [options.ulClass] additional class for the
       * `ul` element that defines the menu.
       * @param {Object} [options.actions] defines the menu items.
       * this is passed to populateMenu()
       * @returns {DIV} the downdown button
       */
      dropdownButton: function(icon, options) {
	if ( !options ) options = {};
	var cls     = options.divClass;
	var ulClass = options.ulClass;

	var dropdown = $.el.div(
	  {class: "btn-group dropdown"+(cls?" "+cls:"")},
	  $.el.button(
	    {class:"dropdown-toggle",
	     "data-toggle":"dropdown"},
	    icon),
	  $.el.ul({class:"dropdown-menu"+(ulClass?" "+ulClass:"")}));

	if ( options.actions )
	  form.widgets.populateMenu($(dropdown), options.client, options.actions);

	return dropdown;
      },

      populateMenu: function(menu, client, actions) {
	var ul = menu.find(".dropdown-menu");
	var data = ul.data("menu")||{};

	function runMenu(ev, a) {
	  var action = $(a).data('action');

	  if ( action )
	    action.call(client, a);
	}

	function addMenuItem(label, onclick) {
	  if ( onclick !== undefined ) {
	    if ( label.indexOf("--") == 0 ) {
	      ul.append($.el.li({class:"divider"}));
	    } else {
	      var a = $.el.a(label);

	      $(a).data('action', onclick);
	      ul.append($.el.li(a));
	    }
	  }
	}

	for(var a in actions) {
	  if ( actions.hasOwnProperty(a) ) {
	    addMenuItem(a, actions[a]);
	  }
	}

	if ( !data.bound ) {
	  data.bound = true;
	  ul.on("click", "a", function(ev) { runMenu(ev, this); } );
	}

	ul.data("menu", data);

	return menu;
      }
    }
  };

  return form;
});
