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

:- module(webstat_predicate,
          [ pi_string_pi/2,                     % +String, -PI
            pred_detail_dict/3                  % :Goal, -Dict, +Options
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(option)).
:- use_module(library(prolog_code)).
:- use_module(library(aggregate)).

:- use_module(webstat(lib/util)).
:- use_module(webstat(lib/stats)).

:- http_handler(webstat('html/predicate/details'), pred_details,
                [id(predicate_details)]).

pred_details(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)]),
                      pi(PIS, [])
                    ]),
    pi_string_pi(PIS, PI),
    pi_head(PI, Pred),
    pred_detail_dict(Pred, Dict, [thread(Thread)]),
    phrase(pred_details(Dict, []), Tokens),
    with_output_to(string(HTML), print_html(Tokens)),
    reply_json(Dict.put(html,HTML)).

pred_details(Dict, Options) -->
    html(table(class([table, 'table-striped', 'pred-details']),
               \pred_detail_rows(Dict, Options))).

pred_detail_rows(Dict, Options) -->
    html([ tr([ th('Properties'),
                td(\pred_props([ multifile,
                                 discontiguous,
                                 dynamic,
                                 thread_local,
                                 incremental
                               ], Dict, Options))
              ]),
           tr([ th('Number of clauses'),
                td(class(count), Dict.clause_count)
              ])
         | \opt_pred_detail_rows(Dict, Options)
         ]).

pred_props([], _, _) --> [].
pred_props([H|T], Dict, Options) -->
    (   { true = Dict.get(H) }
    ->  html(span(class('pred-opt'), H))
    ;   []
    ),
    pred_props(T, Dict, Options).

opt_pred_detail_rows(Dict, Options) -->
    pred_source(Dict, Options),
    pred_tabled(Dict, Options),
    pred_idg(Dict, Options).

pred_source(Dict, _Options) -->
    { _{file:File, line:Line} :< Dict.get(source) },
    !,
    html(tr([ th('Defined at'),
              td([ span(class(file), File),
                   :,
                   span(class(line), Line)
                 ])
            ])).
pred_source(_, _) --> [].

pred_tabled(Dict, _Options) -->
    { Tables = Dict.get(tabled) },
    html(tr([ th('Tabled'),
              td(class(piped),
                 [ \dict_count(Tables, tables,  '~D tables',  true),
                   \dict_count(Tables, answers, '~D answers', true)
                 ])
            ])),
    !.
pred_tabled(_, _) --> [].

pred_idg(Dict, _Options) -->
    { IDG = Dict.get(idg) },
    html(tr([ th('IDG'),
              td(class(piped),
                 [ \dict_count(IDG, affected,  '~D affected',  gt(0)),
                   \dict_count(IDG, dependent, '~D dependent', gt(0))
                 ])
            ])),
    !.
pred_idg(_, _) --> [].

dict_count(Dict, Name, Format, Cond) -->
    { Count = Dict.get(Name),
      (   Cond == true
      ;   Cond = gt(N), Count > N
      )
    },
    !,
    html(span(class('table-prop'), Format-[Count])).
dict_count(_, _, _, _) --> [].


%!  pred_detail_dict(:Goal, -Dict, +Options) is det.

pred_detail_dict(Pred, Dict, Options) :-
    (   predicate_property(Pred, thread_local)
    ;   predicate_property(Pred, tabled),
        \+ predicate_property(Pred, tabled(shared))
    ),
    !,
    option(thread(Thread), Options, main),
    in_thread(Thread, pred_detail_dict_(Pred, Dict, Options)).
pred_detail_dict(Pred, Dict, Options) :-
    pred_detail_dict_(Pred, Dict, Options).

pred_detail_dict_(Pred, Dict, Options) :-
    findall(Name-Value, pred_detail(Pred, Name, Value, Options), Pairs),
    dict_create(Dict, pred, Pairs).

pred_detail(Pred, Name, Value, _Options) :-
    pred_detail(Pred, Name, Value).

pred_detail(Pred, Name, true) :-
    pred_bool_option(Name),
    once(predicate_property(Pred, Name)).
pred_detail(Pred, source, source{file:File,line:Line}) :-
    (   predicate_property(Pred, multifile)
    ->  fail                            % TBD
    ;   predicate_property(Pred, file(File)),
        predicate_property(Pred, line_count(Line))
    ).
pred_detail(Pred, clause_count, Count) :-
    (   predicate_property(Pred, number_of_clauses(Count))
    ->  true
    ;   Count = 0
    ).
pred_detail(Pred, tabled, Tables) :-
    predicate_property(Pred, tabled),
    table_statistics_dict(Pred, Tables0),
    del_dict(variant, Tables0, _, Tables).
pred_detail(Pred, idg, idg{ affected:Affected,
                            dependent:Dependent
                          }) :-
    predicate_property(Pred, tabled),
    predicate_property(Pred, incremental),
    idg_pred_count(Pred, affected, Affected),
    idg_pred_count(Pred, dependent, Dependent).

idg_pred_count(Pred, Dir, Count) :-
    pi_head(PI, Pred),
    aggregate_all(count, idg_predicate_edge(PI, Dir, _, _), Count).

pred_bool_option(multifile).
pred_bool_option(discontiguous).
pred_bool_option(dynamic).
pred_bool_option(thread_local).
pred_bool_option(incremental).


%!  pi_string_pi(+String, -PI)
%
%   Transform a string  [<module>   ":"]<name>("/"|"//")<arity>  into  a
%   predicate indicator term.

pi_string_pi(String, PI) :-
    (   sub_atom(String, _, _, A, '//'),
        sub_atom(String, _, A, 0, ArityS),
        atom_number(ArityS, Arity)
    ->  PI0 = Name//Arity
    ;   sub_atom(String, _, _, A, '/'),
        sub_atom(String, _, A, 0, ArityS),
        atom_number(ArityS, Arity)
    ->  PI0 = Name/Arity
    ),
    (   sub_atom(String, B, _, _, ':'),
        sub_atom(String, 0, B, _, Module)
    ->  PI = Module:PI0
    ;   B = -1,
        PI = PI0
    ),
    B1 is B+1,
    A1 is A+1,
    sub_atom(String, B1, _, A1, Name).
