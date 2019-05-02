{-|
Module      : Language.Python35
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

@hpython@ provides tools for working with Python source code.

"Language.Python.DSL": A DSL for writing Python programs

"Language.Python.Optics": Optics for working with Python syntax trees

"Language.Python.Parse": Parse Python source into a syntax tree

"Language.Python.Render": Pretty print Python syntax trees

"Language.Python.Syntax": The data structures that represent Python programs, like 'Statement' and 'Expr'

"Language.Python.Validate": Validate aspects of Python syntax trees, like indentation, syntax, or scope

-}

module Language.Python35
  ( module Language.Python35.DSL
  , module Language.Python35.Optics
  , module Language.Python35.Parse
  , module Language.Python35.Render
  , module Language.Python35.Syntax
  , module Language.Python35.Validate
  )
where

import Language.Python35.DSL
import Language.Python35.Optics
import Language.Python35.Parse
import Language.Python35.Render
import Language.Python35.Syntax
import Language.Python35.Validate
