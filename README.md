# hpython

Haskell-based language tools for Python

<img src="http://i.imgur.com/0h9dFhl.png" width="300px"/>

`hpython` provides an abstract syntax tree for Python 3.5, along with a parser, printer,
and syntax checker. It also exposes optics for working with the AST, and a DSL for writing
Python programs directly in Haskell.

## Features

* Formatting-preserving syntax tree
* Parser and printer, with round-trip laws: 
  * `print ∘ parse ≡ id`
  * `(parse ∘ print) ∘ (parse ∘ print) ≡ parse ∘ print`
* Optics for manipulating the syntax tree
* Indentation, syntax, and scope checking
* The syntax tree is indexed by its level of validation, to distinguish between syntactically
  valid Python and unvalidated code
* Convenient DSL for building Python programs

## Examples

See the `example` directory

## Development Pipeline

* Support other versions of Python while re-using as much common code as possible
* Style configs for the DSL
* Human readable validation errors, with source spans

## Contribution

Feel free to file an issue or pull request on Github, or contact us at:

IRC - #qfpl on Freenode
Email - <oᴉ˙ldɟb@llǝʞsɐɥ>
