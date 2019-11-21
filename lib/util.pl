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

:- module(webstat_util,
          [ in_thread/2,                % +Thread, :Goal
            to_primitive/2              % +Data, -Primitive
          ]).

:- meta_predicate
    in_thread(+, 0).

%!  in_thread(+Thread, :Goal) is semidet.
%
%   Run Goal as an interrupt in the context  of Thread. This is based on
%   thread_signal/2. If waiting times  out,   we  inject  a stop(Reason)
%   exception into Goal. Interrupts can be   nested, i.e., it is allowed
%   to run an in_thread/2 while the target  thread is processing such an
%   interrupt.

in_thread(Thread, Goal) :-
    thread_self(Thread),
    !,
    once(Goal).
in_thread(Thread, Goal) :-
    term_variables(Goal, Vars),
    thread_self(Me),
    A is random(1 000 000 000),
    thread_signal(Thread, run_in_thread(Goal,Vars,A,Me)),
    catch(thread_get_message(in_thread(A,Result)),
          Error,
          forward_exception(Thread, A, Error)),
    (   Result = true(Vars)
    ->  true
    ;   Result = error(Error)
    ->  throw(Error)
    ;   fail
    ).

run_in_thread(Goal, Vars, Id, Sender) :-
    (   catch_with_backtrace(call(Goal), Error, true)
    ->  (   var(Error)
        ->  thread_send_message(Sender, in_thread(Id, true(Vars)))
        ;   Error = stop(_)
        ->  true
        ;   thread_send_message(Sender, in_thread(Id, error(Error)))
        )
    ;   thread_send_message(Sender, in_thread(Id, false))
    ).

forward_exception(Thread, Id, Error) :-
    kill_with(Error, Kill),
    thread_signal(Thread, kill_task(Id, Kill)),
    throw(Error).

kill_with(time_limit_exceeded, stop(time_limit_exceeded)) :-
    !.
kill_with(_, stop(interrupt)).

kill_task(Id, Exception) :-
    prolog_current_frame(Frame),
    prolog_frame_attribute(Frame, parent_goal,
                           run_in_thread(_Goal, _Vars, Id, _Sender)),
    !,
    throw(Exception).
kill_task(_, _).


%!  to_primitive(+Prolog, -Primitive)
%
%   Translate aribrary Prolog  data  into  something   we  can  pass  as
%   primitive JSON data, for example as a table row.

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
