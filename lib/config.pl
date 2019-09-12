/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2019, VU University Amsterdam
			      CWI, Amsterdam
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

:- module(webstat_config,
	  [ webstat_reply_config/2,	% +Request, +Options
	    webstat_config/2,		% ?Type, ?Config
	    webstat_config_hash/2	% -HASH, +Options
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_json)).
:- use_module(library(option)).
:- use_module(library(apply)).

:- multifile
    config/2,					% ?Key, ?Value
    config/3.					% ?Key, ?Value, +Options

/** <module> Make HTTP locations known to JavaScript code
*/

:- http_handler(webstat('webstat_config.json'),
		webstat_reply_config([]), []).


		 /*******************************
		 *	       CONFIG		*
		 *******************************/

%%	webstat_reply_config(+Options, +Request) is semidet.
%
%	Emit a configuration object to the client if the client requests
%	for '.../webstat_config.json', regardless  of   the  path  prefix.

webstat_reply_config(Options, Request) :-
	option(path(Path), Request),
	file_base_name(Path, 'webstat_config.json'),
	json_config(JSON, Options),
	reply_json(JSON).

%%	webstat_config_hash(-Hash, +Options) is det.
%
%	True if Hash is the SHA1 of the SWISH config.

webstat_config_hash(Hash, Options) :-
	json_config(Config, Options),
	variant_sha1(Config, Hash).

json_config(json{ http: json{ locations:JSON
			    },
		  webstat: SWISHConfig
		}, Options) :-
	http_locations(JSON),
	webstat_config_dict(SWISHConfig, Options).

http_locations(JSON) :-
	findall(ID-Path,
		( http_current_handler(Path, _:_, Options),
		  memberchk(id(ID), Options)
		), Pairs),
	keysort(Pairs, Sorted),
	remove_duplicate_ids(Sorted, Cleaned),
	dict_pairs(JSON, json, Cleaned).

remove_duplicate_ids([], []).
remove_duplicate_ids([Id-Path1,Id-Path2|T], [Id-Path1|Cleaned]) :- !,
	same_ids(T, Id, T1, Paths0),
	sort([Path1,Path2|Paths0], Unique),
	(   Unique = [_]
	->  true
	;   print_message(warning, http(duplicate_handlers(Id, Unique)))
	),
	remove_duplicate_ids(T1, Cleaned).
remove_duplicate_ids([H|T0], [H|T]) :-
	remove_duplicate_ids(T0, T).

same_ids([], _, [], []).
same_ids([Id-Path|T0], Id, T, [Path|TP]) :- !,
	same_ids(T0, Id, T, TP).
same_ids(T, _, T, []).


%%	webstat_config_dict(-Config:dict, +Options) is det.
%
%	Obtain name-value pairs from webstat_config:config/2

webstat_config_dict(Config, Options) :-
	findall(Key-Value, webstat_config(Key, Value, Options), Pairs),
	keysort(Pairs, Sorted),
	warn_duplicate_config(Sorted, Unique),
	dict_pairs(Config, json, Unique).

:- dynamic  warned_duplicate/1.
:- volatile warned_duplicate/1.

warn_duplicate_config([], []).
warn_duplicate_config([K-V1,K-V2|T0], [K-V1|T]) :- !,
	collect_same(K, T0, VL, T1),
	(   warned_duplicate(K)
	->  true
	;   sort([V1,V2|VL], [_])
	->  true
	;   print_message(warning, webstat(duplicate_config(K, [V1,V2|VL]))),
	    assertz(warned_duplicate(K))
	),
	warn_duplicate_config(T1, T).
warn_duplicate_config([KV|T0], [KV|T]) :- !,
	warn_duplicate_config(T0, T).

collect_same(K, [K-V|T0], [V|VT], T) :- !,
	collect_same(K, T0, VT, T).
collect_same(_, List, [], List).

%!	config(-Key, -Value) is nondet.
%!	webstat_config(-Key, -Value) is nondet.
%
%	Define a name/value pair that will end up in the WEBSTAT config
%	object (see `web/js/config.js`)

webstat_config(Key, Value) :-
	webstat_config(Key, Value, []).

webstat_config(Key, Value, Options) :-
	config(Key, Value, Options).
webstat_config(Key, Value, _) :-
	config(Key, Value).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(http(duplicate_handlers(Id, Paths))) -->
	[ 'Duplicate HTTP handler IDs: "~w"'-[Id] ],
	paths(Paths).
prolog:message(webstat(duplicate_config(K, [V0|List]))) -->
	[ 'Duplicate WEBSTAT config values for "~w": ~p.  Using ~q'-
	  [K, [V0|List], V0]
	].

paths([]) --> [].
paths([H|T]) --> [ '\t~q'-[H], nl ], paths(T).
