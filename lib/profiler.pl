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
:- use_module(library(lists)).

:- use_module(webstat(lib/graphviz)).
:- use_module(webstat(lib/util)).
:- use_module(webstat(lib/predicate)).

:- http_handler(webstat_api(profiler/control/Action), prof_control(Action),
                [id(prof_control)]).
:- http_handler(webstat_api('profiler/predicates'), prof_predicates,
                [id(prof_predicates)]).
:- http_handler(webstat_api('profiler/graph'), prof_graph,
                [id(prof_graph)]).

:- dynamic
    profile_data/3.                     % Thread, Data, Time

%!  prof_control(+Action, +Request)
%
%   Control the profiler.

prof_control(Action, Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)])
                    ]),
    in_thread(Thread, prof_do_control(Action, Reply)),
    reply_json(Reply).

prof_do_control(record, json{prev:Old,new:cputime}) :-
    profiler(Old, cputime).
prof_do_control(pause,  json{prev:Old,new:false}) :-
    profiler(Old, false).
prof_do_control(reset,  json{new:false, clear:true}) :-
    thread_id(Id),
    retractall(profile_data(Id, _, _)),
    reset_profiler.

thread_id(Id) :-
    thread_self(Me),
    thread_id(Me, Id).

thread_id(Me, Id) :-
    (   atom(Me)
    ->  Id = Me
    ;   thread_property(Me, id(Id))
    ).

%!  prof_predicates(+Action)
%
%   HTTP handler to show the profiled predicates

prof_predicates(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)])
                    ]),
    in_thread(Thread, profile_data(Data)),
    thread_id(Thread, Id),
    get_time(Now),
    asserta(profile_data(Id, Now, Data)),
    Summary = Data.summary,
    Total is max(0, Summary.ticks-Summary.accounting),
    maplist(pred_row(Total), Data.nodes, Predicates),
    columns(Columns),
    reply_json(json{ summary:Summary,
                     data:Predicates,
                     columns:Columns,
                     table: #{ initialSort: [
                               #{ column: call, dir:desc},
                               #{ column: time_cumulative, dir:desc}
                               ],
                               layout: fitData
                             }
                   }).

columns([
    json{title: "Predicate",
         field: label
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
                           label:Label,
                           time_cumulative:CumPercent,
                           time_self:Percent,
                           call:Call,
                           redo:Redo,
                           exit:Exit,
                           fail:Fail}) :-
    _{ predicate:PI,
       ticks_self:Ticks, ticks_siblings:TicksSiblings,
       call:Call, redo:Redo, exit:Exit
     } :< Node,
    to_primitive(PI, PIs),
    predicate_label(PI, Label),
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
    pi_string_pi(FocusS, Focus),
    thread_id(Thread, Id),
    (   profile_data(Id, _Time, Data)
    ->  true
    ;   in_thread(Thread, profile_data(Data))
    ),
    Summary = Data.summary,
    Total is max(0, Summary.ticks-Summary.accounting),
    call_cleanup(prof_graph(Data, Focus, Graph,
                            [ level(0),
                              total(Total),
                              focus(Focus)
                            ]),
                 retractall(assigned(_,_))),
    reply_graph(Graph, []).

prof_graph(Data, Focus, digraph(Graph), Options) :-
    graph_nodes(Data, Focus, Nodes),
    graph_edges(Data, Nodes, Edges),
    maplist(gv_node(Data, Options), Nodes, GVNodes),
    maplist(gv_edge(Data, Options), Edges, GVEdges),
    append(GVNodes, GVEdges, Graph).

gv_node(Data, Options, PI, Node) :-
    call_node(Data, PI, PNode),
    option(total(Total), Options),
    Perc is 100*(PNode.ticks_self+PNode.ticks_siblings)/Total,
    (   option(focus(PI), Options)
    ->  Extra = [penwidth(5), color(purple)]
    ;   Extra = []
    ),
    profile_node(PI, _NodeID, Node, Extra, [time(Perc)]).

gv_edge(_Data, _Options, edge(F,T,Calls), edge(FId-TId, Attrs)) :-
    node_id(F, FId),
    node_id(T, TId),
    edge_attrs(Calls, Attrs).

edge_attrs(Calls, [label(Label)]) :-
    format(string(Label), '~D', [Calls]).

profile_node(PI, Id,
             node(Id,
                  [ href(URL)
                  | Extra0
                  ]),
             Extra,
             Attrs) :-
    node_id(PI, Id),
    term_string(PI, URL),
    phrase(prof_node_attrs(PI, Attrs), Extra0, Extra).

prof_node_attrs(PI, Attrs) -->
    prof_node_label(PI, Attrs),
    prof_node_tooltip(PI, Attrs),
    prof_node_shape(PI, Attrs).

prof_node_label('<spontaneous>', _) -->
    !,
    [ label(html(i('<spontaneous>'))) ].
prof_node_label(PI, [time(Perc)]) -->
    !,
    [ label(html(table([ border(0),
                         cellpadding(0),
                         color(black)
                       ],
                       [ tr(td(colspan(1), Label)),
                         tr([ td([ border(1),
                                   width(40),
                                   height(14),
                                   fixedsize(true),
                                   bgcolor(BgColor)
                                 ], font('point-size'(10), PercLabel))
                            ])
                       ])))
    ],
    { Frac is max(0.01, Perc/100),
      format(string(BgColor), '#ff8080;~2f:white', [Frac]),
      predicate_label(PI, Label),
      format(string(PercLabel), '~1f%', Perc)
    }.
prof_node_label(PI, _) -->
    [ label(Label),
      tooltip(Label)
    ],
    { predicate_label(PI, Label)
    }.


prof_node_tooltip('<spontaneous>', _) -->
    !,
    [ tooltip('Unknown caller') ].
prof_node_tooltip(PI, [time(Perc)]) -->
    { pred_attrs(PI, PAttrs),
      PAttrs \== [],
      !,
      predicate_label(PI, Label),
      atomics_to_string(PAttrs, ", ", PAttrString),
      format(string(Tooltip), '~w\n~w\n~1f%', [Label, PAttrString, Perc])
    },
    [ tooltip(Tooltip) ].
prof_node_tooltip(PI, [time(Perc)]) -->
    !,
    { predicate_label(PI, Label),
      format(string(Tooltip), '~w\n~1f%', [Label, Perc])
    },
    [ tooltip(Tooltip) ].
prof_node_tooltip(PI, _) -->
    { pred_attrs(PI, PAttrs),
      PAttrs \== [],
      !,
      predicate_label(PI, Label),
      atomics_to_string(PAttrs, ", ", PAttrString),
      format(string(Tooltip), '~w\n~w%', [Label, PAttrString])
    },
    [ tooltip(Tooltip) ].
prof_node_tooltip(PI, _) -->
    { predicate_label(PI, Label)
    },
    [ tooltip(Label) ].


prof_node_shape('<spontaneous>', _) -->
    !,
    [ shape(egg) ].
prof_node_shape(PI, _) -->
    { pi_head(PI, Head) },
    head_node_shape(Head),
    head_node_fill(Head).

head_node_shape(Head) -->
    { predicate_property(Head, dynamic) },
    !,
    [ shape(cylinder) ].
head_node_shape(Head) -->
    { predicate_property(Head, tabled) },
    !,
    [ shape(box3d) ].
head_node_shape(_) -->
    [].

head_node_fill(Head) -->
    { predicate_property(Head, incremental) },
    !,
    [style(filled), fillcolor('#80ff80')].
head_node_fill(_) -->
    [style(filled), fillcolor('#ffffff')].

pred_attrs(PI, Attrs) :-
    pi_head(PI, Head),
    findall(Attr, head_attr(Head, Attr), Attrs).

head_attr(Head, tabled) :-
    predicate_property(Head, tabled).
head_attr(Head, incremental) :-
    predicate_property(Head, incremental).
head_attr(Head, dynamic) :-
    predicate_property(Head, dynamic).

%!  graph_edges(+Data, +Nodes, -Edges) is det.
%
%   Find the edges between the given set of nodes

graph_edges(Data, Nodes, Edges) :-
    findall(Edge, graph_edge(Data, Nodes, Edge), Edges).

graph_edge(Data, Nodes, edge(F,T,Calls)) :-
    member(F, Nodes),
    member(T, Nodes),
    call_edge(Data, T, caller, F, Node),
    arg(5, Node, Calls).

%!  graph_nodes(+Data, +Agenda, -Nodes, +Options) is det.
%
%

graph_nodes(Data, Start, Nodes) :-
    graph_nodes(Data, [Start-0], [], Nodes0),
    sort([Start|Nodes0], Nodes).

graph_nodes(_, Agenda, _, _) :-
    length(Agenda, Len),
    debug(webstat(call_graph), 'Agenda = ~D~n', [Len]),
    fail.
graph_nodes(_, [], _, []) :-
    !.
graph_nodes(Data, [N-_Level|NT], Expanded, Nodes) :-
    memberchk(N, Expanded),
    !,
    graph_nodes(Data, NT, Expanded, Nodes).
graph_nodes(Data, [N-0|NT], Expanded, Nodes) :-
    !,
    callers(Data, N, 0, A1, C1),
    callees(Data, N, 0, A2, C2),
    append([NT,A1,A2], Agenda),
    append(C1, C2, C),
    append(C, T, Nodes),
    graph_nodes(Data, Agenda, [N|Expanded], T).
graph_nodes(Data, [N-Level|NT], Expanded, Nodes) :-
    Level > 0,                                   % callers
    !,
    callers(Data, N, Level, AddAgenda, Selected),
    append(Selected, T, Nodes),
    append(NT, AddAgenda, NewAgenda),
    graph_nodes(Data, NewAgenda, [N|Expanded], T).
graph_nodes(Data, [N-Level|NT], Expanded, Nodes) :-
    Level < 0,                                   % callees
    callees(Data, N, Level, AddAgenda, Selected),
    append(Selected, T, Nodes),
    append(NT, AddAgenda, NewAgenda),
    graph_nodes(Data, NewAgenda, [N|Expanded], T).

callers(Data, N, Level, Agenda, Selected) :-
    findall(Caller, call_edge(Data, N, caller, Caller, _), Callers),
    top_nodes(Data, Level, Callers, Selected),
    debug(webstat(call_edge), 'Callers of ~p are ~p', [N, Selected]),
    NewLevel is Level+1,
    (   (N == 0 ; Selected == [])
    ->  tag_level(Selected, NewLevel, Agenda)
    ;   Selected = [Ex|_],
        tag_level([Ex], NewLevel, Agenda)
    ).

callees(Data, N, Level, Agenda, Selected) :-
    findall(Callee, call_edge(Data, N, callee, Callee, _), Callees),
    top_nodes(Data, Level, Callees, Selected),
    debug(webstat(call_edge), 'Callees of ~p are ~p', [N, Selected]),
    NewLevel is Level-1,
    (   (N == 0 ; Selected == [])
    ->  tag_level(Selected, NewLevel, Agenda)
    ;   Selected = [Ex|_],
        tag_level([Ex], NewLevel, Agenda)
    ).

top_nodes(Data, Level, Nodes, Top) :-
    rank_nodes(Nodes, Data, Ranked),
    sort(1, >=, Ranked, Sorted),
    Max is max(0, 8//(2^abs(Level))),
    debug(webstat(call_graph), 'Level = ~D, max = ~D~n', [Level, Max]),
    length(Sorted, Len),
    (   Len =< Max
    ->  TopP = Sorted
    ;   length(TopP, Max),
        append(TopP, _, Sorted)
    ),
    pairs_values(TopP, Top).

rank_nodes([], _, []).
rank_nodes([H|T0], Data, [C-H|T1]) :-
    call_node(Data, H, PNode),
    !,
    C is PNode.ticks_self + PNode.ticks_siblings,
    rank_nodes(T0, Data, T1).
rank_nodes([H|T0], Data, T1) :-
    debug(webstat(call_graph), 'No info to rank ~p~n', [H]),
    rank_nodes(T0, Data, T1).

tag_level([], _, []).
tag_level([H|T], L, [H-L|T1]) :-
    tag_level(T, L, T1).

%!  call_edge(+Data, ?PI1, ?Dir, ?PI2, -Node) is nondet.

call_edge(Data, PI1, Dir, PI2, Node) :-
    call_node(Data, PI1, PNode),
    (   Dir = callee,
        member(Node, PNode.callees),
        arg(1, Node, PI2)
    ;   Dir = caller,
        member(Node, PNode.callers),
        arg(1, Node, PI2)
    ).

call_node(Data, PI, PNode) :-
    (   ground(PI)
    ->  (   member(PNode, Data.nodes),
            PI = PNode.predicate
        ->  true
        )
    ;   member(PNode, Data.nodes),
        PI = PNode.predicate
    ).

%!  node_id(+PI, -Id) is det.
%
%   Assign node identifiers for predicates.

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
