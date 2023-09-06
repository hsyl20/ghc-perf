# Analyzers

An analyzer provides some information about a project. Some analyzers need to
build projects with some specific compilers, options, etc.

## Module dependencies analyzer

Showing dependency graph

Use ghc -M to dump dependencies between modules?
  - Or use the ghc api to traverse dependencies as graphmod does

Per module code stats
  - size in lines and bytes
  - number of comment lines and of code lines (cf cloc)
  - number of top-level definitions?
  - usage of CPP or other preprocessor (lhs, hsc...)
  - usage of TH

Graph parallelism analysis

- Detect parallelism metrics:
  - length of the longest path
    - in number of modules
    - using module code size as a proxy for build time?
    - link to Build time analyzer to compute and display graph using actual build times

- find bottlenecks
  - modules that have a lot of transitive dependencies
  - suggest splitting them
    - analyze to find what's really depended on in them?
  - suggest reducing their own dependencies so that they compile earlier?

Module loop analyzer
- Detect loops (hs-boot)
  - Suggest removal to allow cross-module inlining!

Automatically propose ways to divide modules based on dependencies between
top-level entities

Case study: use this to fix GHC's
[#19108](https://gitlab.haskell.org/ghc/ghc/-/issues/19108) (removal of hs-boot in GHC)

## Build time analyzer

Build time per module
- get build time per module using one-shot compilation
- handle different sets of flags (e.g. -O0, -O1, -O2 at least)

Graph parallelism analysis
- Graph metrics (as in "Module dependencies analyzer") but using actual build time

Parallel build analyzer
- --make overhead
  - compare make mode with -j1 with one-shot module after module + link
- charts build perf with -j1, -j2, -j3...
  - use threaded RTS with -N

## Static assembly analyzer

Static analysis of GHC produced assembly

Dump assembly code (-ddump-asm)

Call llvm-mca on it:

NCG improvement:
- insert ret instructions after tail calls just to please the analyzers (help finding function boundaries)?

## Static Cmm analyzer

Write analyzer for Cmm code similar to those for C codes

## Execution sampler

Sample running program with perf record
- Support selecting the frequency (e.g. -F max)
- Support selection of counters

Alternative to perf: use ptrace to pause a process and to look into it (access registers, etc.).
- Sylvain has a prototype
- Pro: can have Haskell specific features:
  - take into account RTS state (in FFI or not…)
  - access Haskell stack (not using rbp/rsp registers on x86)
- Con: more work

Can sample:
- GHC building the project
- Runnables of the project

Display results

Explain how to relate assembly code to Cmm code
- Explain where to find Cmm conventions (register assignments, etc.) for a given platform

NCG improvement:
- Add flag to add dwarf info for Cmm instead of Haskell code

Case study:
- slow info table access found when profiling GHC building Cabal with perf

## Detect host sanity

automatically detect/fix platform issues:
- turbo boost enabled...
- CPU charge too high: detect and warn about host excessive activity when we're idle

analysis run at the program start and display warning sign somewhere on the interface

detect measurement stability:
- compile some module a few times
- check that measurements are stable

## Compiler comparison

Compare build times for a given project with different GHC versions

With/without parallelism (-j1, -j2, -j4...) enabled

With/without some flags (optimizations...)

Chart perf per compiler version, flags, etc.

## RTS flags tuner

run the same program with different RTS flags
- GC options
- Heap/stack options
- Threading options

bench performance

genetic algorithm to find a good set of flags?

## Code size analyzer

Reports about code size

Size of:
- build program or library
- objects (.o)
- functions? (look into the object file)

Compare with different build options
- -O0, -O1...
- -fspec-constr
- ...

## GHC phase analyzer

For each module, show time per GHC phase
- cf -ddump-timings -dmachine-readable

Stacked bar chart looks great: see https://github.com/codedownio/time-ghc-modules

Ensure that we deepseq IRs between each phase (with a new flag if needed)
- add Forcing phases between each phase so that we see it on the graph
- ensure (with something like nothunks or a new rts census primitive?) that no thunk remains
- perhaps serialize to temporary iface and then reload?

## Thread analyzer

ThreadScope-like view of events from the eventlog

Global metrics:
- number of threads, max parallelism
- stats per capability
- Report RTS --internal-counters related to threads

Mutator vs RTS time
- FFI analyzer (time spent in FFI calls)

## GC analyzer

Report stats with different GCs
- non-moving
- parallel-gc
- vanilla generational

Report RTS --internal-counters related to GC
- require rts built with PROF_SPIN

Allow tuning of GC options

Doc:
- explain GC options

## Heap analyzer

Show heap activity

Various heap profiling modes

Interactive heap analysis via ghc-debug: http://ghc.gitlab.haskell.org/ghc-debug/
- see what's already in mentioned "ghc-vis" program

Need a GHC with ghc-debug enabled

Ensure that some types don't refer to another instance of themselves
- e.g. HscEnv, DynFlags

Space leak detector
- Add sequence number to heap allocated objects
- Find live thunks referring to very old objects
- Allow dumping current sequence number in event log (to indicate phase transitions)

## Event analyzer

Timeline of events from the eventlog

## Cost centre analyzer

User specified cost centres

Late cost centres

-fprof-auto with a big warning that it modifies the generated code a lot and that its results may be totally off?

## STG analyzer

Show STG ticks (ticky ticky)

Show late cost centres
- See https://well-typed.com/blog/2023/03/prof-late/
- Especially the speedscope output

Show provenance of heap allocations
- -hi -fdistinct-constructor-tables -finfo-table-map

Correlate to STG dumps
- we need to have STG dumps for the GHCs we use
- Show STG code annotated with ticky results
- Similarly to HPC pretty-printer: use colors to show hot spots

## Syscall analyzer

trace syscalls

Show number of syscalls per period of time on a timeline

Metrics:
- number of time each syscall is called
- number of errors per syscall
- cf strace -c

Detection of some specific syscalls
- Blocking syscalls in unsafe FFI calls
- Failing syscalls (e.g. returning EAGAIN)
- Same file opened many times
- ...

## Containers analyzer

Provide a containers package with tracing

Trace life of objects, operations applied to them, etc.
- Uid per container, update uid after every mutation. Track (with uid) removal,
  insertion, lookup, average number of elements, balancing?... Use HasCallStack
  for call origin.

Metrics:
- most commonly used operations
- stats on the size of the containers

## Black hole analyzer

See #9221

Patched GHC to get more tracing about blackholes

Test running with -feager-blackholing

Doc:
- explain black holes
- explain -feager-blackholing
- explain noDuplicate# primop (doc is in rts/PrimOps.cmm)

## Platform analyzer

See #9221

Report topology (similarly to or with lstopo/hwloc)
- Report hyperthreading Cores

Instrumentation
- report when capabilities are moved to different Core
- report NUMA memory stats
  - https://dl.acm.org/doi/10.1145/3471873.3472974

Allow pinning of capabilities to different cores
- Try with/without using hyperthreaded cores

## Core simplifier analyzer

Chart number of terms/types/coercions per phase during simplification

Parse verbose-core2core output to show the different stages

Explore with different options:
- enabling/disabling passes, etc.

## JavaScript analyzer

Build to JS platform

Use specific JS profiling tools

## Control-flow analyzer

Because of tail calls, we don't have access to a call stack

Could we build an external one?
- emit events for tail-calls
- emit events for return-to-stack
- emit events for store continuation into stack

It could be extremely slow but we could store events on disk and build the trace offline with precise timings

Control-flow per thread

## SIMD analyzer

We don't generate SIMD instructions, except with LLVM/C backend

Compare perf between NCG and these backends?

If we improve the NCG to generate SIMD instructions, we could report these optimizations here (as Intel ICC does)

## Malloc/libc analyzer

plug valgrind / cachegring to get some stats about allocations

other libc wrappers?

LD_PRELOAD our own wrapper to trace some calls?

## Code Object analyzer

Report infos about objects, programs, and libraries

cf ldd

Report:
- size
- RPATHS
- ...

## STM analyzer

Report STM uses
- Log of transactions
- Number of aborted transactions
- Average number of TVars per transactions

## Lock analyzer

Report about use of locking primitives (MVar)

Number of threads waiting, etc.

## Interned strings analyzer

Dump FastString table

Evolution of the FastString table over time
- e.g. dump in eventlog after each pass?

Suggest caching common strings
- e.g. “dsN”, cf #17259

## Thunk analyzer

Every location we allocate a thunk we assign a unique number

At runtime we have two boxes per such location:
  - # of allocated thunks
  - # of evaluated (entered) thunks

Then we can report the percentage of thunks created that were evaluated per location
  - Always evaluated: suggest to make it strict (if not loopy…)
  - Never evaluated: could we avoid allocating thunks here at all?

## Sparks analyzer

Report stats about sparks

## StablePtr analyzer

Report about StablePtr usage
  - Could be used to identify leaks

Extend to other GHC roots?
