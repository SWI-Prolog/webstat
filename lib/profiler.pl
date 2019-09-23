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

prof_graph(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)]),
                      focus(FocusS, [])
                    ]),
    pi_string_pi(FocusS, PI),
    pi_head(PI, Focus),
    in_thread(Thread, profile_data(Data)), % TBD: Cache?
    call_cleanup(prof_graph(Data, Focus, Graph),
                 retractall(assigned(_,_))),
    reply_graph(Graph, []).

prof_graph(Data, Focus, digraph(Graph)) :-
    include(is_focus(Focus), Data.nodes, Nodes),
    maplist(prof_graph, Nodes, Graphs),
    append(Graphs, Graph).

prof_graph(Data, [Node|Relatives]) :-
    profile_node(Data.predicate, NodeID, Node, [penwidth(2)]),
    phrase(relatives(Data, NodeID), Relatives).

relatives(Data, To) -->
    relatives(Data.callers, caller, To),
    relatives(Data.callees, callee, To).

relatives([], _, _) --> [].
relatives([node(Pred,_Cycle,_Ticks,_TicksSiblings,
                _Calls, _Redos, _Exits)|T], Dir, To) -->
    { profile_node(Pred, PredID, Node, []),
      edge(Dir, To, PredID, Edge)
    },
    [ Node, Edge ],
    relatives(T, Dir, To).

edge(caller, To, Id, edge(Id-To, [])).
edge(callee, To, Id, edge(To-Id, [])).

is_focus(Focus, Node) :-
    get_dict(predicate, Node, Focus).

profile_node(Head, Id,
             node(Id,
                  [ label(Label),
                    href(URL)
                  | Extra
                  ]),
             Extra) :-
    node_id(Head, Id),
    pi_head(PI, Head),
    term_string(PI, URL),
    format(string(Label), '~q', [PI]).

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
