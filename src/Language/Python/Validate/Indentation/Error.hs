{-# language TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, DataKinds, KindSignatures #-}

{-|
Module      : Language.Python.Validate.Indentation.Error
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Indentation.Error where

-- import Control.Lens.TH
import Control.Lens.Type
import Control.Lens.Prism

import Language.Python.Internal.Syntax


data IndentationError (v :: [*]) a
  = WrongIndent Indent Indent a
  | TabError a
  | ExpectedGreaterThan [Indent] (Indents a)
  | ExpectedEqualTo [Indent] (Indents a)
  | EmptyContinuedLine a
  deriving (Eq, Show)

-- makeClassyPrisms ''IndentationError
class AsIndentationError r_amPW v_aiuW a_aiuX | r_amPW -> v_aiuW
                                                          a_aiuX where
  _IndentationError :: Prism' r_amPW (IndentationError v_aiuW a_aiuX)
  _WrongIndent :: Prism' r_amPW (Indent, Indent, a_aiuX)
  _TabError :: Prism' r_amPW a_aiuX
  _ExpectedGreaterThan :: Prism' r_amPW ([Indent], Indents a_aiuX)
  _ExpectedEqualTo :: Prism' r_amPW ([Indent], Indents a_aiuX)
  _EmptyContinuedLine :: Prism' r_amPW a_aiuX
  _WrongIndent = ((.) _IndentationError) _WrongIndent
  _TabError = ((.) _IndentationError) _TabError
  _ExpectedGreaterThan = ((.) _IndentationError) _ExpectedGreaterThan
  _ExpectedEqualTo = ((.) _IndentationError) _ExpectedEqualTo
  _EmptyContinuedLine = ((.) _IndentationError) _EmptyContinuedLine
instance AsIndentationError (IndentationError v_aiuW a_aiuX) v_aiuW a_aiuX where
  _IndentationError = id
  _WrongIndent
    = (prism
          (\ (x1_amQ3, x2_amQ4, x3_amQ5)
            -> ((WrongIndent x1_amQ3) x2_amQ4) x3_amQ5))
        (\ x_amQ6
            -> case x_amQ6 of
                WrongIndent y1_amQ7 y2_amQ8 y3_amQ9
                  -> Right (y1_amQ7, y2_amQ8, y3_amQ9)
                _ -> Left x_amQ6)
  _TabError
    = (prism (\ x1_amQb -> TabError x1_amQb))
        (\ x_amQc
            -> case x_amQc of
                TabError y1_amQd -> Right y1_amQd
                _ -> Left x_amQc)
  _ExpectedGreaterThan
    = (prism
          (\ (x1_amQf, x2_amQg) -> (ExpectedGreaterThan x1_amQf) x2_amQg))
        (\ x_amQh
            -> case x_amQh of
                ExpectedGreaterThan y1_amQi y2_amQj -> Right (y1_amQi, y2_amQj)
                _ -> Left x_amQh)
  _ExpectedEqualTo
    = (prism
          (\ (x1_amQl, x2_amQm) -> (ExpectedEqualTo x1_amQl) x2_amQm))
        (\ x_amQn
            -> case x_amQn of
                ExpectedEqualTo y1_amQo y2_amQp -> Right (y1_amQo, y2_amQp)
                _ -> Left x_amQn)
  _EmptyContinuedLine
    = (prism (\ x1_amQr -> EmptyContinuedLine x1_amQr))
        (\ x_amQs
            -> case x_amQs of
                EmptyContinuedLine y1_amQt -> Right y1_amQt
                _ -> Left x_amQs)
