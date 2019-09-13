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

:- module(webstat_perfchart, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(webstat(lib/util)).

:- http_handler(webstat_api('perf/sample'), perf_sample, [id(perf_sample)]).
:- http_handler(webstat_api('perf/series'), perf_series, [id(perf_series)]).

%!  perf_series(+Request)
%
%   Return the defined performance series. This is a JSON list providing
%   objects with minimally a `name` field.

perf_series(_Request) :-
    findall(Name-Props, stat_series(Name, Props), Pairs),
    dict_create(Series, json, Pairs),
    reply_json(json{ series:Series,
                     rate:1
                   }).

%!  perf_sample(+Request)
%
%   Return a JSON object providing values for each defined series.

perf_sample(Request) :-
    http_parameters(Request,
                    [ thread(Thread, [default(main)])
                    ]),
    in_thread(Thread, perf_sample_data(Data)),
    reply_json(json{sample:Data}).

stat_series(global,
            _{ label: "Global stack",
               color: "#f00"
             }).
stat_series(trail,
            _{ label: "Trail stack",
               color: "#0f0"
             }).
stat_series(local,
            _{ label: "Local stack",
               color: "#00f"
             }).

perf_sample_data(Data) :-
    findall(Name-Value, active_stat(Name, Value), Pairs),
    dict_create(Data, stat, Pairs).

active_stat(Name, Value) :-
    stat(Name, Value),
    once(stat_series(Name, _)).

stat(global, Value) :-
    statistics(globalused, Value).
stat(trail, Value) :-
    statistics(trailused, Value).
stat(local, Value) :-
    statistics(localused, Value).
