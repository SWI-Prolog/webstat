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

:- module(webstat_table, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(prolog_code)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(solution_sequences)).
:- use_module(library(apply)).

:- use_module(webstat(lib/util)).
:- use_module(webstat(lib/stats)).
:- use_module(webstat(lib/predicate)).

/** <module> HTTP API to dump individual tables
*/

:- http_handler(webstat_api('table/table'),
                show_table, [id(webstat_table)]).

%!  show_table(+Request)
%
%   Dump an individual table.

show_table(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)]),
                      variant(VariantS, []),
                      max(Max, [default(100 000)])
                    ]),
    to_goal(VariantS, Variant),
    in_thread(Thread, table_data(Variant, Data, [max(Max)])),
    reply_json(Data).

to_goal(VariantS, Variant) :-
    term_string(Variant, VariantS).

table_data(Variant, json{ variant:VariantS,
                          status:Status,
                          columns:Columns,
                          answers: Answers
                        },
           Options) :-
    variant_string(Variant, VariantS),
    option(max(Max), Options, 100 000),
    current_table(user:Variant, Trie),          % for now, user is default
    '$tbl_table_status'(Trie, Status, Wrapper, Skeleton),
    findall(t(Wrapper,Delay,I),
            call_nth(limit(Max, '$tbl_answer'(Trie, Skeleton, Delay)), I),
            Triples),
    sort(1, @<, Triples, Sorted),                 % should we sort?
    maplist(answer(user), Sorted, Answers),
    (   has_conditionals(Answers)
    ->  findall(Col, column(Col, wfs), Columns)
    ;   findall(Col, column(Col, nowfs), Columns)
    ).

column(col{title: "Index",
           field: index,
           align: "right"}, _).
column(col{title: "Answer",
           field: answer}, _).
column(col{title: "Condition",
           field: condition}, wfs).

has_conditionals(Answers) :-
    member(A, Answers),
    get_dict(condition, A, _),
    !.

answer(M, t(Answer0,true,I), row{answer:AnswerS,index:I}) :-
    !,
    unqualify(Answer0, M, Answer),
    variant_string(Answer, AnswerS).
answer(M, t(Answer0,Condition,I),
       row{answer:AnswerS, condition:CondS, index:I}) :-
    unqualify(Answer0, M, Answer),
    unqualify(Condition, M, SimpleCondition),
    copy_term(Answer+SimpleCondition,
              AnswerC+SimpleConditionC),
    numbervars(AnswerC+SimpleConditionC, 0, _, [singletons(true)]),
    format(string(AnswerS), '~q', AnswerC),
    format(string(CondS), '~q', SimpleConditionC).

unqualify((A0,B0), M, (A,B)) :-
    !,
    unqualify(A0, M, A),
    unqualify(B0, M, B).
unqualify((A0;B0), M, (A;B)) :-
    !,
    unqualify(A0, M, A),
    unqualify(B0, M, B).
unqualify(tnot(A0), M, tnot(A)) :-
    !,
    unqualify(A0, M, A).
unqualify(M:G, M, G) :-
    !.
unqualify(G, _, G).

variant_string(Variant, String) :-
    copy_term(Variant, Copy),
    numbervars(Copy, 0, _, [singletons(true)]),
    format(string(String), '~q', Copy).
