# SWI-Prolog web-enabled monitor

This project implements web server for   SWI-Prolog  to monitor progress
and examine the status of your program. The current version concentrates
on _tables_ and  notably  _incremental   tabling_.  Future  versions are
likely to integrate several of the development tools, for example:

  - Code dependency view now realised using `gxref/0`.
  - Thread activity and resource usage now done using
    `?- prolog_ide(thread_monitor).`
  - Break down memory usage over stacks, modules, predicates,
    tables, etc.
  - Examine JITI indexes.
  - Dump tables and dynamic predicates.

The  code  heavily  reuses    [SWISH](https://swish.swi-prolog.org).  We
consider this a separate  project,  but   future  versions  of SWISH are
likely to integrate this tool.

## Installation

This project is in a very early state.  To run the prototype:

  - Get the latest version of
    [swipl-devel.git](https://github.com/SWI-Prolog/swipl-devel.git)
  - Get the [yarn](https://yarnpkg.com) package manager and use it
    to install the JavaScript dependencies:

        yarn

## Running

  - Load your (tabled) project
  - Load `server.pl` from this package
  - Run this to start the server on port 4000

	?- webstat(4000).

  - Direct your browser to http://localhost:4000

## Controlling the view

The file `webstat_control.pl` is a lightweight   program that allows you
to control the interface from  your   program.  Probably the most useful
feature is to control the _Resource chart_  tab by starting and stopping
it from your program and optionally add events that indicate progress of
the program you are analyzing.
