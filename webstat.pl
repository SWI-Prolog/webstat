/*  Part of SWI-Prolog webstat

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, CWI Amsterdam
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

:- module(webstat,
          [
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).

:- use_module(library(tstat)).

:- http_handler(webstat(home), webstat_home, []).

webstat_home(_Request) :-
    reply_html_page(
        [ title('SWI-Prolog web statistics')
        ],
        \webstat_home).

webstat_home -->
    webstat_resources.

webstat_resources -->
    webstat_css,
    webstat_js.

webstat_js  --> html_post(head, \include_webstat_js).
webstat_css --> html_post(head, \include_webstat_css).

include_webstat_js -->
	{ webstat_resource(js, JS),
	  webstat_resource(rjs, RJS),
	  http_absolute_location(webstat(js/JS), WebStatJS, []),
	  http_absolute_location(webstat(RJS),   WebStatRJS, [])
	},
	rjs_timeout(JS),
	html(script([ src(WebStatRJS),
		      'data-main'(WebStatJS)
		    ], [])).

rjs_timeout('webstat-min') --> !,
	js_script({|javascript||
// Override RequireJS timeout, until main file is loaded.
window.require = { waitSeconds: 0 };
		  |}).
rjs_timeout(_) --> [].


include_webstat_css -->
	{ webstat_resource(css, CSS),
	  http_absolute_location(webstat(css/CSS), WebStatCSS, [])
	},
	html(link([ rel(stylesheet),
		    href(WebStatCSS)
		  ])).

webstat_resource(Type, ID) :-
	alt(Type, ID, File),
	(   File == (-)
	;   absolute_file_name(File, _P, [file_errors(fail), access(read)])
	), !.

alt(js,  'webstat-min',     webstat_web('js/webstat-min.js')) :-
	\+ debugging(nominified).
alt(js,  'webstat',         webstat_web('js/webstat.js')).
alt(css, 'webstat-min.css', webstat_web('css/webstat-min.css')) :-
	\+ debugging(nominified).
alt(css, 'webstat.css',     webstat_web('css/webstat.css')).
alt(rjs, 'js/require.js', webstat_web('js/require.js')) :-
	\+ debugging(nominified).
alt(rjs, 'node_modules/requirejs/require.js', -).
