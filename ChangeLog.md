# Revision history for hpython

## Next version

* Changed the sigature of `Data.Validation.Monadic.bindVM` to have the same form as
  `(>>=)`
  
* Added `HasAnn` instance for `Language.Python.Syntax.Ann.Ann`

* Reworked scope checking (`Language.Python.Validate.Scope`)

  Instead of the old triplicate `ScopeContext`, we now keep a single context
  and mark each entry with its 'occurrence path'. This is much simpler to manage,
  but just as expressive as the old version.
  
* Improved scope checking of compound statements

  Compound statements like
  
  ```
  if ...:
      x = 1
  else:
      y = x
  ```
  
  were succeeding in scope checking because the variables from each block were
  added to the scope sequentially. Now, the multiple blocks are checked in the
  same scope, and their final scopes are combined at the end of the process.

## 0.2

*2019-01-10*

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
