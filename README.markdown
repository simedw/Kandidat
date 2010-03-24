Optimization under runtime
=========================
This is a bachelor project at Chalmers, Sweden, where we implement a small stg like language and try to apply optimization of partial applied function during runtime.

Dan, Daniel, Olle, Simon

Build
=====

    % cabal configure -ftest -fbenchmark
    % cabal build

Benchmarks
==========

Make sure to build fegr with the benchmark flag.
To get started run

    % dist/build/benchmark/benchmark --help

Testsuite
=========

Make sure to build fegr with the test flag.

    % cabal test

It should hopefully pass all the tests.
