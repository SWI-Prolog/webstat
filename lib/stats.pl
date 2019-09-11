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
            table_statistics_dict/2		% :Head, -Dict
          ]).
:- use_module(library(aggregate)).
:- use_module(library(error)).

/** <module> Data collection utilities
*/

:- meta_predicate
    tabled_predicate_with_tables(:).

%!  tabled_predicate_with_tables(:Pred) is nondet.
%
%   True if Pred is a tabled predicate and has at least one table.

tabled_predicate_with_tables(Pred) :-
    predicate_property(Pred, tabled),
    \+ predicate_property(Pred, imported_from(_)),
    \+ \+ table(Pred, _).

%!  table_statistics_dict(:Goal, -Dict) is det.
%
%   True when Dict represents summary information for all tables that
%   are subsumed by Goal.

table_statistics_dict(Goal, Dict) :-
    findall(Stat-Value, table_statistics(Goal, Stat, Value), Pairs),
    dict_create(Dict, table_stat, [variant-Goal|Pairs]).

%!  table_statistics(:Goal, ?Key, ?Value) is nondet.

table_statistics(Variant, Stat, Value) :-
    (   var(Stat)
    ->  table_statistics_(Variant, Stat, Value)
    ;   table_statistics_(Variant, Stat, Value)
    ->  true
    ).

table_statistics_(Variant, tables, NTables) :-
    aggregate_all(count, table(Variant, _), NTables).
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
hidden_stat(invaldated, Variant) :-
    callable(Variant),
    \+ predicate_property(Variant, tabled(incremental)).
hidden_stat(reevaluated, Variant) :-
    callable(Variant),
    \+ predicate_property(Variant, tabled(incremental)).

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
