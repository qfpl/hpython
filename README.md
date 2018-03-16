# hpython

Haskell-based language tools for Python

<img src="http://i.imgur.com/0h9dFhl.png" width="300px"/>

## `hpython` contains proofs of concept for a few of ideas:

### Validation Stages

Using phantom types and type-level sets to re-use the same abstract syntax tree, but
recieve different validation guarantees. A Python program begins in the "unvalidated"
state (type-level empty set). Then, increasing levels of validation are imposed, and its
level of validation is updated to reflect this (members are added to the set).

### Optics-based Refactoring

Prisms for the syntax tree: prisms can be used to match on a tree with any validation level,
but can only be used to construct unvalidated terms. We can't be sure that we didn't build
an invalid tree.

Where possible, datatypes in the abstract syntax tree have instances of
[Plated](https://hackage.haskell.org/package/lens/docs/Control-Lens-Plated.html). Alongside
the prisms, this provides a powerful API for rewriting arbitrary terms.

### Python 2 and 3 compatibility

There is a subset of Python programs which are compatible with both Python 2 and 3. It would
be nice if we could talk about this difference at the type level.

Coming soon (maybe).
