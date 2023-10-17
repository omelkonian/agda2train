agda2train: An Agda backend to generate training data for machine learning
===========================================================================
[![CI](https://github.com/omelkonian/agda2train/workflows/CI/badge.svg)](https://github.com/omelkonian/agda2train/actions) [![Hackage](https://repology.org/badge/version-for-repo/hackage/haskell:agda2train.svg)](http://hackage.haskell.org/package/agda2train)

This is work in progress and a neural network trained on these data to provide
*premise selection* is under way.

## How to run

The `agda2train` package is published on Hackage, so one can simply:
```bash
$ cabal install agda2train
$ agda2train SomeFile.agda
```

Alternatively, assuming a working Haskell installation (`cabal` available),
one can clone this repo and use the provided Makefile to build the package locally,
as well as run our test suite:
```bash
$ git clone git@github.com:omelkonian/agda2train.git
$ cd agda2train
$ make build # build package
$ make install # install `agda2train` executable
$ make test # run the test-suite (based on golden files in `test/golden/*`)
$ make repl # REPL for developers
$ make allTests # extract JSON data from all example files in `test/*`
$ make stdlib # extract JSON data from Agda's standard library
```

## Relevant Agda issues

- [Type-on-hover #516](https://github.com/agda/agda/issues/516)
- [Step-by-step evaluation #4747](https://github.com/agda/agda/issues/4747)
- [Reverting projection-like optimization #5142](https://github.com/agda/agda/pull/5142/)
