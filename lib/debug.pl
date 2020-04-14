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

:- module(webstat_debug, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(solution_sequences)).
:- use_module(library(debug)).

:- use_module(util).
:- use_module(push).

:- dynamic (active/1) as volatile.

:- http_handler(webstat_api('debug/topics'),  topic_table,   [id(debug_topics)]).
:- http_handler(webstat_api('debug/topic'),   topic,         [id(debug_topic)]).
:- http_handler(webstat_api('debug/forward'), forward_debug, [id(debug_forward)]).
:- http_handler(webstat_api('debug/reset'),   reset_debug,   [id(debug_reset)]).

%!  topic_table(+Request) is det.
%
%   Emit the known debug topics and their status as a JSON array

topic_table(_Request) :-
    findall(Topic, debug_topic(Topic), Topics),
    reply_json(json{topics:Topics}).

debug_topic(json{id: Id, topic:Primitive, active:On}) :-
    call_nth(debugging(Topic, On), Id),
    to_primitive(Topic, Primitive).

topic(Request) :-
    http_parameters(Request,
                    [ topic(TopicS, []),
                      active(Active, [boolean])
                    ]),
    term_string(Topic, TopicS),
    (   Active == true
    ->  debug(Topic)
    ;   nodebug(Topic)
    ),
    reply_json(json{topic:TopicS, active:Active}).

forward_debug(Request) :-
    http_parameters(Request,
                    [ val(Active, [boolean])
                    ]),
    retractall(active(_)),
    asserta(active(Active)),
    reply_json(Active).

reset_debug(_Request) :-
    flag('$debug_msg_no', _, 0),
    reply_json(true).

:- multifile
    prolog:debug_print_hook/3.

prolog:debug_print_hook(Topic, Format, Args) :-
    active(true),
    flag('$debug_msg_no', N, N+1),
    SegNo is N+1,
    format(string(Message), Format, Args),
    to_primitive(Topic, TopicS),
    thread_id(Id),
    get_time(Now),
    webstat_push(json{target:debug,
                      id:SegNo,
                      thread:Id,
                      time:Now,
                      topic:TopicS,
                      message:Message}).

thread_id(Id) :-
    thread_self(Me),
    (   atom(Me)
    ->  Id = Me
    ;   thread_property(Me, id(Id))
    ).
