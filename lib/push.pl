/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(webstat_push,
          [ webstat_push/1
          ]).
:- use_module(library(http/hub)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(debug)).

:- http_handler(webstat_api(push), webstat_push_handler, [ id(webstat_push) ]).

webstat_push_handler(Request) :-
    http_upgrade_to_websocket(
        add_to_hub,
        [ guarded(false),
          subprotocols(['v1.webstat.swi-prolog.org', webstat])
        ],
        Request).

add_to_hub(WebSocket) :-
    create_webstat_hub,
    hub_add(webstat_hub, WebSocket, _WSID).

create_webstat_hub :-
    current_hub(webstat_hub, _), !.
create_webstat_hub :-
    with_mutex(webstat_push, create_webstat_hub_sync).

create_webstat_hub_sync :-
    current_hub(webstat_hub, _), !.
create_webstat_hub_sync :-
    hub_create(webstat_hub, Room, _{}),
    thread_create(webstat_hub(Room), _, [alias(webstat_hub)]).

webstat_hub(Room) :-
    (   catch(webstat_event(Room), E, webstat_exception(E))
    ->  true
    ;   print_message(warning, goal_failed(webstat_event(Room)))
    ),
    webstat_hub(Room).

webstat_exception('$aborted') :- !.
webstat_exception(E) :-
    print_message(warning, E).

webstat_event(Room) :-
    thread_get_message(Room.queues.event, Message),
    (   handle_message(Message, Room)
    ->  true
    ;   print_message(warning, goal_failed(handle_message(Message, Room)))
    ).

handle_message(Message, Room) :-
    debug(webstat(push), 'Got ~p in ~p', [Message, Room]).


%!  webstat_push(+Message) is det.
%
%   Push a message to the listening browser(s).

webstat_push(Message) :-
    current_hub(webstat_hub, _),
    !,
    hub_broadcast(webstat_hub, json(Message), all).

all(_).
