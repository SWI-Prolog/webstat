/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2020, VU University Amsterdam
                              CWI, Amsterdam
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

:- module(webstat_perfchart, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(broadcast)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- if(exists_source(library(mallocinfo))).
:- use_module(library(mallocinfo)).
:- endif.
:- if(exists_directory('/proc/self/fd')).
:- use_module(procps).
:- endif.

:- use_module(util).
:- use_module(push).

:- multifile
    webstat:stat_series/2,
    webstat:stat_value/2,
    webstat:stat_disabled/1.

:- http_handler(webstat_api('perf/sample'), perf_sample, [id(perf_sample)]).
:- http_handler(webstat_api('perf/series'), perf_series, [id(perf_series)]).

%!  perf_series(+Request)
%
%   Return the defined performance series. This is a JSON list providing
%   objects with minimally a `name` field.

perf_series(_Request) :-
    findall(Series, active_stat_series(_, Series), SeriesList),
    reply_json(json{ series:SeriesList,
                     rate:1,
                     refresh:1
                   }).

active_stat_series(Name, Dict) :-
    webstat:stat_series(Name, Props),
    \+ webstat:stat_disabled(Name),
    Dict = Props.put(name, Name).
active_stat_series(Name, Dict) :-
    stat_series(Name, Props),
    \+ webstat:stat_disabled(Name),
    Dict = Props.put(name, Name).


		 /*******************************
		 *           MARKINGS		*
		 *******************************/

:- listen(webstat(perfchart, Action),
          wstat(Action)).

wstat(Action) :-
    must_be(ground, Action),
    (   is_list(Action)
    ->  maplist(action_json, Action, JSON)
    ;   action_json(Action, JSON0),
        JSON = [JSON0]
    ),
    webstat_push(JSON).

action_json(marking(Options),
            json{target:perfchart,
                 marking: Obj
                }) :-
    !,
    dict_create(Obj0, json, Options),
    format_label(Obj0, Obj).
action_json(interval(Time),
            json{target:perfchart,
                 interval:Time
                }) :-
    !,
    must_be(number, Time).
action_json(refresh(Time),
            json{target:perfchart,
                 refresh:Time
                }) :-
    !,
    must_be(number, Time).
action_json(series(Series),
            json{target:perfchart,
                 series:JSON
                }) :-
    must_be(list, Series),
    convlist(series_json, Series, JSON).
action_json(Action,
            json{target:perfchart,
                 action:Action
                }) :-
    !,
    must_be(oneof([stop,start,clear]), Action).


format_label(Options0, Options) :-
    Fmt-Args = Options0.get(label),
    !,
    format(string(Label), Fmt, Args),
    Options = Options0.put(label, Label).
format_label(Options0, Options) :-
    Label0 = Options0.get(label),
    \+ (string(Label0); atom(Label0)),
    !,
    term_string(Label0, Label),
    Options = Options0.put(label, Label).
format_label(Options, Options).

series_json(+Series, json{series:Series, show:true}) :-
    stat_series(Series, _),
    !.
series_json(-Series, json{series:Series, show:false}) :-
    stat_series(Series, _),
    !.
series_json(Term, _) :-
    domain_error(series, Term).


		 /*******************************
		 *            SAMPLING		*
		 *******************************/

%!  perf_sample(+Request)
%
%   Return a JSON object providing values for each defined series.

perf_sample(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)])
                    ]),
    in_thread(Thread, perf_sample_data(Data)),
    reply_json(json{sample:Data}).

:- discontiguous
    stat_series/2,
    stat/2.

stat_series(global,
            _{ label: "Global stack",
               unit:  bytes
             }).
stat_series(trail,
            _{ label: "Trail stack",
               unit:  bytes
             }).
stat_series(local,
            _{ label: "Local stack",
               unit:  bytes
             }).
stat_series(stack,
            _{ label: "Total stack usage",
               unit:  bytes,
               active: true
             }).

stat_series(thread_cpu,
            _{ label: "Thread CPU",
               unit:  percent,
               zero_ok: true
             }).
stat_series(process_cpu,
            _{ label: "Process CPU",
               unit:  percent,
               zero_ok: true,
               active: true
             }).

stat_series(atoms,
            _{ label: "Atoms"
             }).
stat_series(functors,
            _{ label: "Functors"
             }).

stat_series(modules,
            _{ label: "Modules"
             }).
stat_series(predicates,
            _{ label: "Predicates"
             }).
stat_series(clauses,
            _{ label: "clauses"
             }).
stat_series(code_mem,
            _{ label: "Memory for VM code",
               unit:  bytes
             }).

stat_series(tables,
            _{ label: "Tables"
             }).
stat_series(table_space,
            _{ label: "Table space",
               unit:  bytes,
               active: true
             }).

perf_sample_data(Data) :-
    findall(Name-Value, active_stat(Name, Value), Pairs),
    dict_create(Data, stat, Pairs).

active_stat(Name, Value) :-
    (   stat(Name, Value),
        once(stat_series(Name, _))
    ;   webstat:stat_value(Name, Value),
        once(webstat:stat_series(Name, _))
    ),
    \+ webstat:stat_disabled(Name).

:- thread_local
    prev_sample/3.

stat(thread_cpu, Value) :-
    \+ webstat:stat_disabled(thread_cpu),
    get_time(Wall1),
    statistics(cputime, CPU1),
    debug(profiler, 'stat(thread_cpu, ~p)', [CPU1]),
    (   retract(prev_sample(thread_cputime, Wall0, CPU0))
    ->  asserta(prev_sample(thread_cputime, Wall1, CPU1)),
        Value is round(100*(CPU1-CPU0)/(Wall1-Wall0))
    ;   asserta(prev_sample(thread_cputime, Wall1, CPU1)),
        Value is 0
    ).
stat(process_cpu, Value) :-
    \+ webstat:stat_disabled(process_cpu),
    get_time(Wall1),
    statistics(process_cputime, CPU1),
    (   retract(prev_sample(process_cputime, Wall0, CPU0))
    ->  asserta(prev_sample(process_cputime, Wall1, CPU1)),
        Value is round(100*(CPU1-CPU0)/(Wall1-Wall0))
    ;   asserta(prev_sample(process_cputime, Wall1, CPU1)),
        Value = 0
    ).


stat(global, Value) :-
    statistics(globalused, Value).
stat(trail, Value) :-
    statistics(trailused, Value).
stat(local, Value) :-
    statistics(localused, Value).
stat(stack, Value) :-
    statistics(stack, Value).

stat(atoms, Value) :-
    statistics(atoms, Value).
stat(functors, Value) :-
    statistics(functors, Value).

stat(modules, Value) :-
    statistics(modules, Value).
stat(predicates, Value) :-
    statistics(predicates, Value).
stat(clauses, Value) :-
    statistics(clauses, Value).
stat(code_mem, Bytes) :-
    statistics(codes, Codes),
    current_prolog_flag(address_bits, Bits),
    Bytes is Codes*(Bits/8).

stat(tables, Value) :-
    \+ webstat:stat_disabled(tables),
    aggregate_all(sum(C), ('$tbl_variant_table'(VariantTrie),
                           trie_property(VariantTrie, value_count(C))),
                  Value).
stat(table_space, Value) :-
     statistics(table_space_used, Value).

:- if(current_predicate(malloc_property/1)).
stat(tcheap, Heap) :-
    statistics(heapused, Heap).

stat(tcheap_fragmentation, Lost) :-
    malloc_property('generic.heap_size'(Total)),
    malloc_property('generic.current_allocated_bytes'(Allocated)),
    Lost is Total-Allocated.

stat_series(tcheap,
            _{ label: "Heap memory",
               title: "Heap (malloc - stack)",
               unit:  bytes
             }).
stat_series(tcheap_fragmentation,
            _{ label: "Malloc lost",
               title: "Freed memory not reused",
               unit:  bytes
             }).
:- endif.

:- if(current_predicate(mallinfo/1)).
stat_series(malloc,
            _{ label: "Malloc in use",
               title: "Malloc'ed memory in use",
               unit:  bytes
             }) :-
    \+ webstat:stat_disabled(mallinfo).
stat_series(mfree,
            _{ label: "Malloc free",
               title: "Freed memory not reused",
               unit:  bytes
             }) :-
    \+ webstat:stat_disabled(mallinfo).
:- if(current_predicate(procps_stat/1)).
stat_series(rss,
            _{ label: "RSS memory",
               title: "Resident Set Size",
               unit:  bytes
             }) :-
    \+ webstat:stat_disabled(procps).
stat_series(heap,
            _{ label: "Heap memory",
               title: "RSS - stacks - free",
               unit:  bytes
             }) :-
    \+ webstat:stat_disabled(mallinfo),
    \+ webstat:stat_disabled(procps).
:- endif.

% Overall memory usage stats. Only for Linux.  Note that mallinfo() is a
% GNU extension and does not report  values   larger  than  4Gb. It also
% tends to get really slow if  the   amount  of allocated memory becomes
% large. Add the line below to your   project  to disable using mallinfo
% based stats.
%
%     webstat:stat_disabled(mallinfo)
%
% RSS is extracted from the Linux /proc  file system and can be disabled
% using
%
%     webstat:stat_disabled(procps)

stat(Stat, Value) :-
    (   (   webstat:stat_disabled(malloc),
            webstat:stat_disabled(mfree),
            webstat:stat_disabled(heap)
        ;   webstat:stat_disabled(mallinfo)
        )
    ->  (   (   webstat:stat_disabled(rss)
            ;   webstat:stat_disabled(procps)
            )
        ->  fail
        ;   procps_stat(ProcPS),
            Stat = rss,
            Value is ProcPS.rss
        )
    ;   mallinfo(Dict),
        mallinfo_stat(Stat, Dict, Value)
    ).

mallinfo_stat(malloc, Dict, Value) :-
    Value = Dict.uordblks.
mallinfo_stat(mfree, Dict, Value) :-
    Value = Dict.fordblks.
:- if(current_predicate(procps_stat/1)).
mallinfo_stat(Stat, Mallinfo, Value) :-
    \+ webstat:stat_disabled(procps),
    procps_stat(ProcPS),
    procps_mallinfo_stat(Stat, Mallinfo, ProcPS, Value).

procps_mallinfo_stat(heap, Mallinfo, ProcPS, Value) :-
    statistics(stack, Stack),
    Value is ProcPS.rss - Mallinfo.fordblks - Stack.
procps_mallinfo_stat(rss, _Mallinfo, ProcPS, Value) :-
    Value is ProcPS.rss.
:- else.
procps_stat(_) :- fail.
:- endif.
:- endif.
