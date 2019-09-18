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

:- module(webstat_tables, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(prolog_code)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- use_module(webstat(lib/util)).
:- use_module(webstat(lib/stats)).
:- use_module(webstat(lib/predicate)).

/** <module> HTTP API for information about predicates
*/

:- http_handler(webstat_api('table/tables'),
                tables, []).

tables(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)]),
                      predicate(PredS, [optional(true)]),
                      goal(GoalS, [optional(true)]),
                      max(Max, [default(1000)])
                    ]),
    to_goal(PredS, GoalS, Goal),
    in_thread(Thread, table_data(Goal, Data, [max(Max)])),
    reply_json(Data).

to_goal(PredS, _GoalS, Goal) :-
    nonvar(PredS),
    !,
    pi_string_pi(PredS, PI),
    pi_head(PI, Goal).
to_goal(_PredS, GoalS, Goal) :-
    nonvar(GoalS),
    !,
    term_string(Goal, GoalS).
to_goal(_PredS, _GoalS, _:_).

table_data(Goal, Tables, Options) :-
    option(max(Max), Options, 1000),
    findall(Dict, table_data_dict(Goal, Dict), Data),
    length(Data, Count),
    (   Count > Max
    ->  sort(answers, >=, Data, Sorted),
        length(Tables, Max),
        append(Tables, _, Sorted)
    ;   Tables = Data
    ).

table_data_dict(Goal, Dict) :-
    table(Goal, Table),
    findall(Name-Value, table_property(Goal, Table, Name, Value), Pairs),
    variant_string(Goal, String),
    dict_create(Dict, json, [variant-String|Pairs]).

table_property(Goal, Table, Name, Value) :-
    (   predicate_property(Goal, incremental)
    ->  incr_table_propdef(Name, Value0, Prop)
    ;   table_propdef(Name, Value0, Prop)
    ),
    answer_table_property(Table, Prop),
    Value is Value0.                    % may be an arithmetic expression

incr_table_propdef(Name, Value, Prop) :-
    table_propdef(Name, Value, Prop).
incr_table_propdef(invalidated, Value, invalidated(Value)).
incr_table_propdef(reevaluated, Value, reevaluated(Value)).

table_propdef(answers,         Value, value_count(Value)).
table_propdef(space,           Value, size(Value)).
table_propdef(duplicate_ratio, Value, duplicate_ratio(Value)).
table_propdef(space_ratio,     Value, space_ratio(Value)).
table_propdef(complete_call,   Value, gen_call_count(Value)).
table_propdef(compiled_space,  Value, compiled_size(Value)).
table_propdef(variables,       Value, variables(Value)).

variant_string(Variant, String) :-
    copy_term(Variant, Copy),
    numbervars(Copy, 0, _, [singletons(true)]),
    format(string(String), '~q', Copy).
