# Features

Note that the project is still at an early stage: none of this has been
implemented yet.

## UIs

### Web UI

Use HTMX to avoid dealing with statelessness.

### Terminal UI

Terminal interface.

Based on https://hackage.haskell.org/package/brick to begin with (see also https://hackage.haskell.org/package/brick-tabular-list for a nice tabular widget we could use).

Pro:

- it can be used remotely (e.g. via SSH)
- compared to a Web interface: no need to deal with HTTP statelessness
- compared to a Web interface: no need to redirect ports

Cons:

- Limited graphics (but we could generate image files)

## Custom GHC builds

Several analyzers below require a custom GHC build, or some files optionally
generated during a GHC build (e.g. STG dumps). The tool will provide its own
reproducible recipes to build these compiler instances instead of relying on
externally provided ones.

Download specific GHC version (git hash)

- custom builds with different flavours
  - profiled
  - ticky
  - patched with ghc-debug init
  - ...

- Patched GHCs are provided as different Git branches to build from:
  - the tool doesn’t maintain patches locally.

- custom builds provide metadata
  - e.g. dump-stg-final for the compiler and the boot libraries
  - for each builds, we know the capabilities (supported flags....) and the quirks of the compiler
  - we can adapt or disable some analysis with some compilers

## Projects

“Projects” are Haskell projects to analyze. The tool will only allow profiling projects.

Project definition contains:
- Recipe to get sources (cabal unpack, git...)
- GHC build flags (not cabal): include dirs, etc.
- Optional runnables: built programs that can be analyzed too

We want to have some project presets. For example:
- HelloWorld
- Cabal: not many deps, buildable by several GHC versions
  - Runnable: cabal solving some build plan
- GHC perf test cases
- no-fib benchmarks
- GHC itself
- we need some project that uses TH (lens based one?)

some currently inefficient test cases (Generics on large datatypes, etc.)

For the first iterations, projects will be defined statically in code. In the future we can imagine having some ghc-prof.yaml files describing projects, but this isn’t a priority.

## Session

For a good UX, a user shouldn’t lose his work so far because of a crash. The tool will serialize its state so that results of previous analysis aren’t lost.

Auto-save session so that results are never lost and can be easily resumed

Associate compiler id to outputs so that we don't mingle outputs produced by different compilers

Log of the different analyzes that have been run
