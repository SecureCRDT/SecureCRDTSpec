
# Secure CRDT Specification

This repository holds an Haskell reference implementation that closely follows the formal specification presented in the paper.
In particular, it includes:

* A library of secure CRDTs
* A secure CRDT client interface that allows simulating the run of a distributed secure CRDT in two worlds:

	- In the ideal world, each replica is emulated by a single party that executes secure operations over plain-text values.
	- In the real world, each replica is emulated by a fixed set of 3 parties that execute secure operations among them (this mode is only for demonstration purposes, and offers no actual MPC security).

## Instructions

1. Inside the `SecCRDT` folder, load the package with cabal:

```
$ cabal repl
```

2. Load the examples module and run an existing test, e.g.:

```
ghci> :m Secure.CRDT.Examples
ghci> testGC2
```
