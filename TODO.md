# SWI-Prolog webstat TODO list

  - Add program push events to the interface, so we can (for example)
    add a vertical line in the resource chart at a specific point.

# Charts

  - Select thread

# Profiler

  - Fix redo/fail counts
  - Hide calls
    - $toplevel can be completely hidden
    - meta predicates can use `noprofile`.  This should however place
      a node for the meta-predicate below the current child.
    - System predicates can be hidden.
  - Better view of focussed call graph
    - Add more levels to expensive calls
    - Add relations between callers

# Memory breakdown

  - Stacks
  - Tables
  - Program
    - Modules
    - Predicate

# Threads

  - Live thread view
    - Status and main statistics
    - Get stack trace
      - Using thread_signal/2

# Indexes

  - jiti_list/0,1
  - (and/or) overall predicate list with
    - Module
    - Clause count
    - Memory
    - Indexes
    - Flags (dynamic, tabled, incremental, shared, ...)

# Checks

  - check/0
  - check_installation/0

# Code overview

  - gxref/0
    - Using prolog code walker instead of source
    - Module dependency graph
    - Predicate zoom in

# Debug messages

  - debug/1,3
  - table with channels
  - table with timestamp, channel, message
