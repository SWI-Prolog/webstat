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

:- module(webstat_profiler,
          [
          ]).
:- use_module(library(statistics)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(prolog_code)).

:- use_module(webstat(lib/graphviz)).
:- use_module(webstat(lib/util)).
:- use_module(webstat(lib/predicate)).

:- http_handler(webstat_api('profiler/predicates'), prof_predicates,
                [id(prof_predicates)]).
:- http_handler(webstat_api('profiler/graph'), prof_graph,
                [id(prof_graph)]).

prof_predicates(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)])
                    ]),
    in_thread(Thread, profile_data(Data)),
    Summary = Data.summary,
    Total is max(0, Summary.ticks-Summary.accounting),
    maplist(pred_row(Total), Data.nodes, Predicates),
    columns(Columns),
    reply_json(json{ summary:Summary,
                     data:Predicates,
                     columns:Columns,
                     table: #{ initialSort: [
                               #{ column: time_cumulative, dir:desc} ],
                               layout: fitData
                             }
                   }).

columns([
    json{title: "Predicate",
         field: predicate
        },
    json{title: "Time",
         field: time_cumulative,
         sorter: number,
         formatter: "money",
         formatterParams: #{precision:1},
         align:right
        },
    json{title: "Self",
         field: time_self,
         sorter: number,
         formatter: "money",
         formatterParams: #{precision:1},
         align:right
        },
    json{title: "Calls",
         field: call,
         sorter: number,
         formatter: "money",
         formatterParams: #{thousand:",", precision:false},
         align:right
        },
    json{title: "Redos",
         field: redo,
         sorter: number,
         formatter: "money",
         formatterParams: #{thousand:",", precision:false},
         align:right
        },
    json{title: "Exits",
         field: exit,
         sorter: number,
         formatter: "money",
         formatterParams: #{thousand:",", precision:false},
         align:right
        },
    json{title: "Failures",
         field: fail,
         sorter: number,
         formatter: "money",
         formatterParams: #{thousand:",", precision:false},
         align:right
        }
]).

pred_row(Total, Node, json{predicate:PIs,
                           time_cumulative:CumPercent,
                           time_self:Percent,
                           call:Call,
                           redo:Redo,
                           exit:Exit,
                           fail:Fail}) :-
    _{ predicate:Pred,
       ticks_self:Ticks, ticks_siblings:TicksSiblings,
       call:Call, redo:Redo, exit:Exit
     } :< Node,
    pi_head(PI, Pred),
    to_primitive(PI, PIs),
    (   Total =:= 0
    ->  Percent = 0,
        CumPercent = 0
    ;   Percent is 100*Ticks/Total,
        CumPercent is 100*(Ticks+TicksSiblings)/Total
    ),
    Fail is Call+Redo-Exit.

%!  prof_graph(+Request)
%
%   HTTP handler to emit a call graph   with profile data around a given
%   _focus_ node.

prof_graph(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)]),
                      focus(FocusS, [])
                    ]),
    pi_string_pi(FocusS, PI),
    pi_head(PI, Focus),
    in_thread(Thread, profile_data(Data)), % TBD: Cache?
    Summary = Data.summary,
    Total is max(0, Summary.ticks-Summary.accounting),
    call_cleanup(prof_graph(Data, Focus, Graph, [total(Total)]),
                 retractall(assigned(_,_))),
    reply_graph(Graph, []).

prof_graph(Data, Focus, digraph(Graph), Options) :-
    include(is_focus(Focus), Data.nodes, Nodes),
    maplist(prof_graph(Options), Nodes, Graphs),
    append(Graphs, Graph).

prof_graph(Options, Data, [Node|Relatives]) :-
    option(total(Total), Options),
    Perc is 100*(Data.ticks_self+Data.ticks_siblings)/Total,
    profile_node(Data.predicate, NodeID, Node, [penwidth(2)],
                 [time(Perc)]),
    phrase(relatives(Data, NodeID, Options), Relatives).

relatives(Data, To, Options) -->
    relatives(Data.callers, caller, To, Options),
    relatives(Data.callees, callee, To, Options).

relatives([node('<recursive>',_Cycle,_Ticks,_TicksSiblings,
                Calls, _Redos, _Exits)|T], Dir, To, Options) -->
    !,
    { edge_attrs(Calls, Attrs) },
    [ edge(To-To, [labeltooltip('Recursive calls')|Attrs]) ],
    relatives2(T, Dir, To, Options).
relatives(Nodes, Dir, To, Options) -->
    relatives2(Nodes, Dir, To, Options).

relatives2(Nodes, Dir, To, Options) -->
    { order_nodes(Nodes, Ordered),
      (   length(Ordered, Len),
          Len > 6
      ->  length(List, 5),                      % skip at least 2
          append(List, Skipped, Ordered)
      ;   List = Ordered
      )
    },
    relative_nodes(List, Dir, To, Options),
    (   { var(Skipped) }
    ->  []
    ;   skipped_edge(Skipped, Dir, To)
    ).

skipped_edge(Skipped, Dir, To) -->
    { length(Skipped, Len),
      format(string(Label), 'Skipped ~D', [Len]),
      format(atom(Id), 'skip-~w-~w', [Dir, To]),
      edge(Dir, To, Id, Edge)
    },
    [ node(Id, [label(Label), penwidth(0)]),
      edge(Edge, [arrowhead(none), style(dotted)])
    ].


relative_nodes([], _, _, _) --> [].
relative_nodes([node(Pred,_Cycle,Ticks,TicksSiblings,
                Calls, _Redos, _Exits)|T], Dir, To, Options) -->
    { option(total(Total), Options),
      Perc is 100*(Ticks+TicksSiblings)/Total,
      profile_node(Pred, PredID, Node, [], [time(Perc)]),
      edge(Dir, To, PredID, Calls, Edge)
    },
    [ Node, Edge ],
    relative_nodes(T, Dir, To, Options).

edge(Dir, To, Id, Calls, edge(Edge, Attrs)) :-
    edge(Dir, To, Id, Edge),
    edge_attrs(Calls, Attrs).

edge(caller, To, Id, Id-To).
edge(callee, To, Id, To-Id).

edge_attrs(Calls, [label(Label)]) :-
    format(string(Label), '~D', [Calls]).

is_focus(Focus, Node) :-
    get_dict(predicate, Node, Focus).

profile_node(Head, Id,
             node(Id,
                  [ label(Label),
                    href(URL)
                  | Extra0
                  ]),
             Extra,
             Attrs) :-
    node_id(Head, Id),
    pi_head(PI, Head),
    term_string(PI, URL),
    pi_label(PI, Label, Attrs),
    shape(Head, Extra0, Extra).

pi_label(PI, html([Label, br([]),
                   font('point-size'(10), PercLabel)]), [time(Perc)]) :-
    !,
    pi_label_string(PI, Label),
    format(string(PercLabel), '~1f%', Perc).
pi_label(PI, Label, _) :-
    pi_label_string(PI, Label).

shape(Head, [shape(cylinder), tooltip('Dynamic predicate')|T], T) :-
    predicate_property(Head, dynamic),
    !.
shape(Head, [style(filled), color(green), tooltip('Tabled predicate')|T], T) :-
    predicate_property(Head, tabled),
    !.
shape(_, T, T).

order_nodes(List, Ordered) :-
    map_list_to_pairs(key_order, List, Keyed),
    sort(1, >=, Keyed, KeyOrdered),
    pairs_values(KeyOrdered, Ordered).

key_order(node(_Pred, _Cycle,Ticks,TicksSiblings,
               _Calls, _Redos, _Exits), Key) :-
    Key is Ticks+TicksSiblings.

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
