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

:- module(webstat_idg,
          [
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(debug)).

:- use_module(webstat(lib/graphviz)).
:- use_module(webstat(lib/util)).
:- use_module(webstat(lib/stats)).

/** <module> Vizualize the IDG
*/

:- http_handler(webstat_api('table/IDG'), idg, []).

idg(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)]),
                      focus(Focus, [optional(true)])
                    ]),
    include(ground, [focus(Focus)], Options),
    in_thread(Thread, idg_graph(Graph, Options)),
    reply_graph(Graph, []).

idg_graph(Graph, Options) :-
    call_cleanup(
        idg_graph_(Graph, Options),
        retractall(assigned(_,_))).

idg_graph_(digraph(Graph), Options) :-
    debug(idg, 'Creating IDG from options = ~p', [Options]),
    findall(PI, idg_predicate(PI), Preds),
    maplist(predicate_node, Preds, Nodes),
    findall(Edge, predicate_edge(Preds, Edge), Edges),
    append([Nodes|Edges], Graph).

predicate_node(P, node(Id, [label(Label)])) :-
    node_id(P, Id),
    term_string(P, Label).

predicate_edge(Preds,
               [ edge(IDFrom-IDTo, [penwidth(W), label(Count)])
               | More
               ]) :-
    member(P, Preds),
    node_id(P, IDFrom),
    debug(idg, '  ~p~n', [P]),
    idg_predicate_edge(P, dependent, P2, Count),
    debug(idg, '     ~p~n', [P2]),
    W is 1+log10(Count),
    (   assigned(P2, IDTo)
    ->  More = []
    ;   node_id(P2, IDTo),
        term_string(P2, Label),
        More = [node(IDTo, [label(Label), shape(cylinder)])]
    ).

:- thread_local assigned/2.

node_id(P, Id) :-
    assigned(P, Id),
    !.
node_id(P, Id) :-
    (   predicate_property(assigned(_,_), number_of_clauses(N))
    ->  N2 is N + 1,
        atom_concat(n, N2, Id)
    ;   Id = n1
    ),
    assertz(assigned(P, Id)).

