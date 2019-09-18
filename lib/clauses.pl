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

:- module(webstat_clauses, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(prolog_code)).

:- use_module(webstat(lib/predicate)).
:- use_module(webstat(lib/util)).

:- http_handler(webstat_api('clause/fact_table'), fact_table,
                [id(fact_table)]).

%!  fact_table(+Request)
%
%   Dump the clauses of a predicate as a table.

fact_table(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)]),
                      pi(PIS, [])
                    ]),
    pi_string_pi(PIS, PI),
    pi_head(PI, Pred),
    fact_table(Thread, Pred, Table),
    reply_json(Table).

fact_table(Thread, Pred, Table) :-
    predicate_property(Pred, thread_local),
    !,
    in_thread(Thread, fact_table(Pred, Table)).
fact_table(_Thread, Pred, Table) :-
    fact_table(Pred, Table).

fact_table(Pred, json{columns:Columns, data:Data}) :-
    Pred = _M:Head,
    Head =.. [_Name|Args],
    arg_names(Args, 1, ColPairs, Columns, Convert),
    dict_create(Dict, row, ColPairs),
    findall(Dict, (Pred,Convert), Data).

arg_names([], _, [], [], true).
arg_names([H|T], I, [Name-HS|DT], [json{title:Title, field:Name}|CT], Cond) :-
    atom_concat(arg_, I, Name),
    format(string(Title), 'Arg ~d', [I]),
    I2 is I+1,
    arg_names(T, I2, DT, CT, Cond0),
    mkconj(to_primitive(H,HS), Cond0, Cond).

:- public
    to_primitive/2.

to_primitive(H, H) :-
    number(H),
    !.
to_primitive(H, H) :-
    atom(H),
    !.
to_primitive(H, H) :-
    string(H),
    !.
to_primitive(V, S) :-
    numbervars(V, 0, _, [singletons(true)]),
    format(string(S), '~p', [V]).
