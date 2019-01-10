# Revision history for hpython

## 0.2

* Improved Plated instance for exprs

  It now drills down into collections, parameters, arguments, subscripts, and
  comprehensions
  
* Added `_Idents` traversal

* Annotations are now wrapped in the `Ann` type to aid generic deriving

* Added `HasExprs` instance for `Module`

* Added `HasStatements` instance for `Statement`

* Added IO-based `read-` functions to `Language.Python.Parse`

* Re-export `Data.Validation` from `Language.Python.Parse`

* Added `annot` and `annot_` lenses to `Language.Python.Syntax.Ann` to retrieve
  annotations from structures

## 0.1.0.1

*2019-01-07*

Welcome to 2019! (Fixed dates)

## 0.1

*2019-01-07*

Initial release
