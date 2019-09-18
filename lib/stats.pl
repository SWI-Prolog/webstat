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

:- module(webstat_stats,
          [ tabled_predicate_with_tables/1,	% ?Head
            dynamic_incremental_predicate/1,	% ?Head
            table_statistics/3,                 % :Goal, ?Key, ?Value
            table_statistics_dict/2,		% :Head, -Dict
            idg_predicate/1,			% -PI
            idg_predicate_edge/4		% +From, +Dir, -To, -Count
          ]).
:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(prolog_code)).

/** <module> Data collection utilities
*/

:- meta_predicate
    tabled_predicate_with_tables(:),
    dynamic_incremental_predicate(:),
    table_statistics_dict(:, -),
    idg_predicate_edge(:,+,-,-).

%!  tabled_predicate_with_tables(:Pred) is nondet.
%
%   True if Pred is a tabled predicate and has at least one table.

tabled_predicate_with_tables(Pred) :-
    predicate_property(Pred, tabled),
    \+ predicate_property(Pred, imported_from(_)),
    \+ \+ table(Pred, _).

%!  dynamic_incremental_predicate(Pred) :-
%
%   True if Pred is a dynamic incremental predicate

dynamic_incremental_predicate(Pred) :-
    predicate_property(Pred, incremental),
    predicate_property(Pred, dynamic),
    \+ predicate_property(Pred, imported_from(_)).


%!  table_statistics_dict(:Goal, -Dict) is det.
%
%   True when Dict represents summary information for all tables that
%   are subsumed by Goal.

table_statistics_dict(Goal, Dict) :-
    findall(Stat-Value, table_statistics(Goal, Stat, Value), Pairs),
    dict_create(Dict, table_stat, [variant-Goal|Pairs]).

%!  table_statistics(:Goal, ?Key, ?Value) is nondet.
%
%   Provide statistics on the predicate referenced by Goal.

table_statistics(Variant, Stat, Value) :-
    (   var(Stat)
    ->  table_statistics_(Variant, Stat, Value)
    ;   table_statistics_(Variant, Stat, Value)
    ->  true
    ).

table_statistics_(Variant, tables, NTables) :-
    aggregate_all(count, table(Variant, _), NTables).
table_statistics_(Variant, clauses, NClauses) :-
    \+ predicate_property(Variant, tabled),
    predicate_property(Variant, number_of_clauses(NClauses)).
table_statistics_(Variant, invalid, NTables) :-
    aggregate_all(count,
                  ( table(Variant, ATrie),
                    '$idg_falsecount'(ATrie, FC),
                    FC > 0
                  ), NTables).
table_statistics_(Variant, Stat, Total) :-
    variant_trie_stat(Stat, _What),
    \+ hidden_stat(Stat, Variant),
    (   avg(Stat)
    ->  aggregate_all(sum(Ratio)+count,
                      variant_stat(Stat, Variant, Ratio),
                      Sum+Count),
        Count > 0,
        Total is Sum/Count
    ;   aggregate_all(sum(Count), variant_stat(Stat, Variant, Count), Total)
    ).

hidden_stat(variables, _).
hidden_stat(lookup, _).
hidden_stat(_, Variant) :-
    \+ callable(Variant),
    !,
    fail.
hidden_stat(invalidated, Variant) :-
    \+ predicate_property(Variant, incremental).
hidden_stat(reevaluated, Variant) :-
    \+ predicate_property(Variant, tabled(incremental)).
hidden_stat(answers,         Variant) :- \+ predicate_property(Variant, tabled).
hidden_stat(size,            Variant) :- \+ predicate_property(Variant, tabled).
hidden_stat(compiled_size,   Variant) :- \+ predicate_property(Variant, tabled).
hidden_stat(value_count,     Variant) :- \+ predicate_property(Variant, tabled).
hidden_stat(space_ratio,     Variant) :- \+ predicate_property(Variant, tabled).
hidden_stat(lookup_count,    Variant) :- \+ predicate_property(Variant, tabled).
hidden_stat(duplicate_ratio, Variant) :- \+ predicate_property(Variant, tabled).
hidden_stat(gen_call_count,  Variant) :- \+ predicate_property(Variant, tabled).
hidden_stat(complete_call,   Variant) :- \+ predicate_property(Variant, tabled).
hidden_stat(compiled_space,  Variant) :- \+ predicate_property(Variant, tabled).

avg(space_ratio).
avg(duplicate_ratio).

variant_stat(Stat, V, Count) :-
    variant_trie_stat(Stat, _, Count, Property),
    table(V, T),
    atrie_prop(T, Property).

atrie_prop(T, size(Bytes)) :-
    '$trie_property'(T, size(Bytes)).
atrie_prop(T, compiled_size(Bytes)) :-
    '$trie_property'(T, compiled_size(Bytes)).
atrie_prop(T, value_count(Count)) :-
    '$trie_property'(T, value_count(Count)).
atrie_prop(T, space_ratio(Values/Nodes)) :-
    '$trie_property'(T, value_count(Values)),
    Values > 0,
    '$trie_property'(T, node_count(Nodes)).
atrie_prop(T, lookup_count(Count)) :-
    '$trie_property'(T, lookup_count(Count)).
atrie_prop(T, duplicate_ratio(Ratio)) :-
    '$trie_property'(T, value_count(Values)),
    Values > 0,
    '$trie_property'(T, lookup_count(Lookup)),
    Ratio is (Lookup - Values)/Values.
atrie_prop(T, gen_call_count(Count)) :-
    '$trie_property'(T, gen_call_count(Count)).
atrie_prop(T, invalidated(Count)) :-
    '$trie_property'(T, invalidated(Count)).
atrie_prop(T, reevaluated(Count)) :-
    '$trie_property'(T, reevaluated(Count)).
atrie_prop(T, falsecount(Count)) :-
    '$idg_falsecount'(T, Count).
atrie_prop(T, variables(Count)) :-
    '$tbl_table_status'(T, _Status, _Wrapper, Skeleton),
    functor(Skeleton, ret, Count).

variant_trie_stat(Stat, What) :-
    (   variant_trie_stat(Stat, What, _, _)
    *-> true
    ;   domain_error(tstat_key, Stat)
    ).

variant_trie_stat(answers,        "Number of answers",
                  Count, value_count(Count)).
variant_trie_stat(duplicate_ratio,"Duplicate answer ratio",
                  Ratio, duplicate_ratio(Ratio)).
variant_trie_stat(space_ratio,    "Space efficiency",
                  Ratio, space_ratio(Ratio)).
variant_trie_stat(complete_call,  "Calls to completed tables",
                  Count, gen_call_count(Count)).
variant_trie_stat(invalidated,    "Times the tables were invalidated",
                  Count, invalidated(Count)).
variant_trie_stat(reevaluated,    "Times the tables were reevaluated",
                  Count, reevaluated(Count)).
variant_trie_stat(space,          "Memory usage for answer tables",
                  Bytes, size(Bytes)).
variant_trie_stat(compiled_space, "Memory usage for compiled answer tables",
                  Bytes, compiled_size(Bytes)).
variant_trie_stat(variables,      "Number of variables in answer skeletons",
                  Count, variables(Count)).

%!  table(:Goal, -AnswerTable) is nondet.
%
%   True when AnswerTable is a trie for a variant subsumed by Goal.

table(M:Variant, Trie) :-
    '$tbl_variant_table'(VariantTrie),
    trie_gen(VariantTrie, M:Variant, Trie).


		 /*******************************
		 *             IDG		*
		 *******************************/

%!  idg_predicate_edge(:PI, +Dir, :PI2, -Count) is nondet.
%
%   True if PI links to  PI2  in   direction  Dir  (one of `affected` or
%   `dependent`). Count represents the number of  linked variants of the
%   two predicates.

idg_predicate_edge(PI, Dir, PI2, Count) :-
    aggregate(count, pred_idg_link_(PI, Dir, PI2), Count).

pred_idg_link_(PI, Dir, PI2) :-
    pi_head(PI, Head),
    table(Head, Trie),
    '$idg_edge'(Trie, Dir, Trie2),
    '$tbl_table_status'(Trie2, _Status, Head2, _Return),
    pi_head(PI2, Head2).

%!  idg_predicate(:PI) is nondet.
%
%   True when PI refers to a tabled predicate that is part of the IDG.

idg_predicate(PI) :-
    Head = _:_,
    tabled_predicate_with_tables(Head),
    predicate_property(Head, tabled(incremental)),
    (   table(Head, Trie),
        '$idg_edge'(Trie, _Dir, _Trie2)
    ->  pi_head(PI, Head)
    ).

