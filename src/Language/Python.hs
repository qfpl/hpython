{-|
Module      : Language.Python
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

@hpython@ provides tools for working with Python source code.

@Language.Python.DSL@:

    A DSL for writing Python programs

@Language.Python.Optics@:

    Optics for working with Python sytnax trees

@Language.Python.Parse@:

    Parse Python source into a syntax tree

@Language.Python.Render@:

    Pretty print Python syntax trees

@Language.Python.Syntax@:

    The data structures that represent Python programs, like 'Statement' and 'Expr'

@Language.Python.Validate@:

    Validate aspects of Python syntax trees, like indentation, syntax, or scope

-}

module Language.Python
  ( module Language.Python.DSL
  , module Language.Python.Optics
  , module Language.Python.Parse
  , module Language.Python.Render
  , module Language.Python.Syntax
  , module Language.Python.Validate
  )
where

import Language.Python.DSL
import Language.Python.Optics
import Language.Python.Parse
import Language.Python.Render
import Language.Python.Syntax
import Language.Python.Validate