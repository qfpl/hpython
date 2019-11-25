# hpython

Haskell-based language tools for Python

![CSIRO's Data61 Logo](https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png)

`hpython` provides an abstract syntax tree for Python 3.5, along with a parser, printer,
and syntax checker. It also contains optics for working with the AST, and a DSL for writing
Python programs directly in Haskell.

## Features

* Formatting-preserving syntax tree
* Parser and printer, with a round-trip law: `print ∘ parse ≡ id`
* Optics for manipulating the syntax tree
* Indentation, syntax, and scope checking
* The syntax tree is indexed by its level of validation, to distinguish between
  syntactically valid Python and unvalidated code
* Convenient DSL for building Python programs

## Examples

See [the `example` directory](https://github.com/qfpl/hpython/tree/master/example)

## FAQ

*Why not just use `language-python`?*

There are two main reasons: 

1. We think the print-parse identity is important. `language-python` discards lexical 
   information like indentation levels and spacing, which means there is big cost to
   using it to modify human-written code. `hpython` retains formatting information
   in a way that has minimal impact when using the library. This means that program
   transformations change as little formatting as possible.
   
2. We want to use types to precisely model the domain. `language-python` unifies
   Python 2 and 3 into a single data structure. We disagree with this choice,
   because Python 2 and 3 have different, non-compatible features. In Haskell terms,
   they are different datatypes with a large amount of overlap. Our goal is to make
   this difference visible in the type system without increasing code repetition.
   
   There are other minor places where `language-python` has made similar concessions,
   like in the treatment of 'starred expressions' (which are not really expressions
   at all).
   

## Development Pipeline

* Support other versions of Python while re-using as much common code as possible
* Style configs for the DSL
* Human readable validation errors, with source spans

## Contribution

Feel free to file an issue or pull request on Github, or contact us at:

IRC - #qfpl on Freenode
Email - <oᴉ˙ldɟb@llǝʞsɐɥ>
