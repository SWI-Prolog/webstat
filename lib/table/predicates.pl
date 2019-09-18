/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(webstat_table_predicates,
          [
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(prolog_code)).

:- use_module(webstat(lib/util)).
:- use_module(webstat(lib/stats)).

/** <module> HTTP API for information about predicates
*/

:- http_handler(webstat_api('table/predicates'),
                table_predicates, []).

table_predicates(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)])
                    ]),
    in_thread(Thread, predicate_data(Data)),
    reply_json(Data).

predicate_data(Data) :-
    Pred = (_:_),
    findall(Dict, predicate_data_dict(Pred, Dict), Data).

predicate_data_dict(Pred, Dict) :-
    (   tabled_predicate_with_tables(Pred)
    ;   dynamic_incremental_predicate(Pred)
    ),
    table_statistics_dict(Pred, Dict0),
    pred_to_json_dict(Dict0, Dict).

pred_to_json_dict(Dict0, Dict) :-
    Head = Dict0.variant,
    pi_head(PI, Head),
    term_string(PI, String),
    (   predicate_property(Head, incremental)
    ->  Incr = true
    ;   Incr = false
    ),
    (   predicate_property(Head, tabled(shared))
    ->  Shared = true
    ;   Shared = false
    ),
    (   predicate_property(Head, tabled(subsumptive))
    ->  Subs = true
    ;   Subs = false
    ),
    (   predicate_property(Head, dynamic)
    ->  Dyn = true
    ;   Dyn = false
    ),
    Dict = Dict0.put(_{ variant:String,
                        incremental:Incr,
                        shared:Shared,
                        subsumptive:Subs,
                        dynamic:Dyn
                      }).

