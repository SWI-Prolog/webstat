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
:- use_module(library(option)).
:- use_module(library(pairs)).

:- use_module(webstat(lib/graphviz)).
:- use_module(webstat(lib/util)).
:- use_module(webstat(lib/stats)).
:- use_module(webstat(lib/predicate)).

/** <module> Vizualize the IDG
*/

:- http_handler(webstat_api('table/IDG'), idg, [id('IDG')]).

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
    option(focus(Focus), Options),
    !,
    (   atomic(Focus)
    ->  pi_string_pi(Focus, PI)
    ;   PI = Focus
    ),
    focussed_idg(PI, Graph, Options).
idg_graph_(digraph(Graph), Options) :-
    debug(idg, 'Creating IDG from options = ~p', [Options]),
    findall(PI, idg_predicate(PI), Preds),
    maplist(predicate_node, Preds, Nodes),
    findall(Edge, predicate_edge(Preds, dependent, Edge, _), Edges),
    append([Nodes|Edges], Graph).

focussed_idg(Focus, [FocusNode|Graph], _Options) :-
    predicate_node(Focus, FocusNode, [penwidth(2)]),
    findall(Edge-P2, predicate_edge([Focus], dependent, Edge, P2), DepPairs),
    findall(Edge-P2, predicate_edge([Focus], affected,  Edge, P2), AffPairs),
    pairs_keys_values(DepPairs, DepEdges, Deps),
    pairs_keys_values(AffPairs, AffEdges, Affs),
    append(Deps, Affs, Expanded0),
    delete(Expanded0, Focus, Expanded),
    inter_edges(Expanded, InterEdges),
    append(DepEdges, AffEdges, NEdges),
    append([InterEdges|NEdges], Graph).

predicate_node(PI, Node) :-
    predicate_node(PI, Node, []).

predicate_node(PI, node(Id,
                        [ label(Label),
                          href(URL)
                        | Shape
                        ]), Attrs) :-
    term_string(PI, URL),
    node_id(PI, Id),
    shape(PI, Shape, Attrs),
    pi_label(PI, Label).

predicate_edge(Preds, Dir,
               [ edge(Edge,
                      [ penwidth(W), label(Count),
                        labeltooltip(Tooltip),
                        edgetooltip(Tooltip)
                      ])
               | More
               ], P2) :-
    edge_dir(Dir, IDFrom, IDTo, Edge),
    member(P, Preds),
    node_id(P, IDFrom),
    debug(idg, '  ~p', [P]),
    idg_predicate_edge(P, Dir, P2, Count),
    (   Dir == affected
    ->  P \== P2
    ;   true
    ),
    debug(idg, '     ~p', [P2]),
    W is 1+log10(Count),
    edge_tooltip(P, Dir, P2, Count, Tooltip),
    (   assigned(P2, IDTo)
    ->  More = []
    ;   node_id(P2, IDTo),
        predicate_node(P2, Node),
        More = [Node]
    ).

edge_dir(dependent, IDFrom, IDTo, IDFrom-IDTo).
edge_dir(affected,  IDFrom, IDTo, IDTo-IDFrom).

inter_edges(Preds, Edges) :-
    findall(Edge, inter_edge(Preds, Edge), Edges).

inter_edge(Preds, edge(IDFrom-IDTo, [penwidth(W), label(Count)])) :-
    member(P1, Preds),
    member(P2, Preds),
    P1 @> P2,
    idg_predicate_edge(P1, dependent, P2, Count),
    node_id(P1, IDFrom),
    node_id(P2, IDTo),
    W is 1+log10(Count).

%!  shape(+PI, -Attrs, ?Tail)

shape(P, [shape(cylinder), tooltip('Incremental Dynamic predicate')|T], T) :-
    pi_head(P, Goal),
    \+ predicate_property(Goal, tabled),
    !.
shape(P, [style(filled), color(orange), tooltip(Tooltip)|T], T) :-
    pi_head(P, Head),
    table_statistics(Head, invalid, Count),
    Count > 0,
    format(string(Tooltip), 'Predicate has ~D invalid tables', [Count]),
    !.
shape(_, [tooltip('IDG node')|T], T).


%!  pi_label(+PI, -Label)
%
%   Generate a (normally) HTML label for a node of the IDG.

pi_label(PI, HTML) :-
    (   pi_head(PI, Head),
        findall(L, label_attr(Head, L), Ls),
        Ls \== [],
        append_sep(Ls, ', ', Attrs)
    ->  HTML = html([Label, br([]) | Attrs])
    ;   HTML = Label
    ),
    pi_label_string(PI, Label).

label_attr(Head, [i('I:'), Inval]) :-
    table_statistics(Head, invalidated, Inval),
    Inval > 0.
label_attr(Head, [i('R:'), Reval]) :-
    table_statistics(Head, reevaluated, Reval),
    Reval > 0.

append_sep([], _, []).
append_sep([H], _, H) :-
    !.
append_sep([H|T], Sep, List) :-
    append(H, [Sep|T1], List),
    append_sep(T, Sep, T1).

edge_tooltip(P1, dependent, P1, Count, Tooltip) :-
    !,
    pi_label_string(P1, L1),
    format(string(Tooltip),
           '~w has ~D self dependencies', [L1, Count]).
edge_tooltip(P1, dependent, P2, Count, Tooltip) :-
    !,
    pi_label_string(P1, L1),
    pi_label_string(P2, L2),
    format(string(Tooltip),
           '~w depends on ~w\nwith ~D variants', [L1, L2, Count]).
edge_tooltip(P1, affected, P2, Count, Tooltip) :-
    edge_tooltip(P2, dependent, P1, Count, Tooltip).

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

