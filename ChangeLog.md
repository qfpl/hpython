# Revision history for hpython

## Next Release

* Improved Plated instance for exprs [breaking]

  It now drills down into collections, parameters, arguments, subscripts, and
  comprehensions
  
* Added `_Idents` traversal [non-breaking]

* Annotations are now wrapped in the `Ann` type to aid generic deriving [breaking]

* Added `HasExprs` instance for `Module` [breaking]

* Added `HasStatements` instance for `Statement` [breaking]

* Added IO-based `read-` functions to `Language.Python.Parse` [non-breaking]

* Re-export `Data.Validation` from `Language.Python.Parse` [breaking]

* Added `annot` and `annot_` lenses to `Language.Python.Syntax.Ann` to retrieve
  annotations from structures [non-breaking]

## 0.1.0.1

*2019-01-07*

Welcome to 2019! (Fixed dates)

## 0.1

*2019-01-07*

Initial release
