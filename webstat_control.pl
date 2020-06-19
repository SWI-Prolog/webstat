/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
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

:- module(webstat_control,
          [ ws_control/2                % +Target, +Action
          ]).
:- use_module(library(broadcast)).

/** <module> Control the webstat interface from Prolog

This module allows applications to  control   the  webstat  interface by
opening  windows,  setting  configuration    parameters  and  start/stop
sampling. It uses the broadcast publish/subscripe library to talk to the
webstat application itself. This implies that   this  code can be loaded
into an application with minimal overhead and   the code only has effect
if the webstat application is also loaded.

Here is a simple example

```
run(Data) :-
    ws_control(perfchart, [clear,interval(5),start]),
    initialize,
    ws_control(perfchart, marking([label="Loading data"])),
    load_data(Data),
    ws_control(perfchart, marking([label="Solve"])),
    solve,
    ws_control(perfchart, marking([label="Report"])),
    report,
    ws_control(perfchart, stop),
```
*/

%!  ws_control(+Target, +Action)
%
%   Control some aspect of  the  webstat   interface.  This  is  notably
%   intended to open windows and control what   they show. Target is the
%   target interface component and Action describes  some action on this
%   component.  Defined targets are:
%
%     - perfchart
%       Manage the performance chart.  Action is one of
%       - start
%         Start sampling
%       - stop
%         Stop sampling
%       - clear
%         Clear all charts
%       - interval(+Seconds)
%         Time between samples in seconds.
%       - refresh(+Seconds)
%         Refresh the window at most every Seconds.  Default is 1.
%         This avoids flooding the browser on higher sampling
%         frequences.
%       - marking(+Options)
%         Add a marking (vertical line) to the chart at the current
%         time.  This can be used to indicate specific stages of the
%         computation and see how the behaviour changes.  Options is
%         a dict or valid input for dict_create/3.  All options are
%         optional.  Defined options are:
%         - color(+CSSColor)
%           Color of the line.  Default is "#000".
%         - width(+Pixels)
%           With of the line.  Default 1.
%         - y(+Ylg10)
%           Log10 value of the Y value for the label.  Default is 11,
%           100G.
%         - label(+Label)
%           Label to show near the marker line.  Label is a string,
%           atom, a term `Fmt-Args` (calling format/3) or an arbitrary
%           Prolog term which is translated into a string using
%           term_string/2.
%        - series(+List)
%          Show/hide series.  List is a list of +Name and -Name terms.
%
%     - window
%       Add a tab to the main window.  Action is one of
%       - perfchart
%       - profiler
%       - messages
%       - tabled_predicates
%       - idg


ws_control(Target, Action) :-
    broadcast(webstat(Target, Action)).
