{-# language TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, DataKinds, KindSignatures #-}
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
  deriving (Eq, Show)

-- makeClassyPrisms ''IndentationError
class AsIndentationError r_aaVz0 v_aaVtZ a_aaVu0 | r_aaVz0 -> v_aaVtZ
                                                              a_aaVu0 where
  _IndentationError ::
    Control.Lens.Type.Prism' r_aaVz0 (IndentationError v_aaVtZ a_aaVu0)
  _WrongIndent ::
    Control.Lens.Type.Prism' r_aaVz0 (Indent, Indent, a_aaVu0)
  _TabError :: Control.Lens.Type.Prism' r_aaVz0 a_aaVu0
  _ExpectedGreaterThan ::
    Control.Lens.Type.Prism' r_aaVz0 ([Indent], Indents a_aaVu0)
  _ExpectedEqualTo ::
    Control.Lens.Type.Prism' r_aaVz0 ([Indent], Indents a_aaVu0)
  _WrongIndent = ((.) _IndentationError) _WrongIndent
  _TabError = ((.) _IndentationError) _TabError
  _ExpectedGreaterThan = ((.) _IndentationError) _ExpectedGreaterThan
  _ExpectedEqualTo = ((.) _IndentationError) _ExpectedEqualTo
instance AsIndentationError (IndentationError v_aaVtZ a_aaVu0) v_aaVtZ a_aaVu0 where
  _IndentationError = id
  _WrongIndent
    = (Control.Lens.Prism.prism
          (\ (x1_aaVz6, x2_aaVz7, x3_aaVz8)
            -> ((WrongIndent x1_aaVz6) x2_aaVz7) x3_aaVz8))
        (\ x_aaVz9
            -> case x_aaVz9 of
                WrongIndent y1_aaVza y2_aaVzb y3_aaVzc
                  -> Right (y1_aaVza, y2_aaVzb, y3_aaVzc)
                _ -> Left x_aaVz9)
  _TabError
    = (Control.Lens.Prism.prism (\ x1_aaVze -> TabError x1_aaVze))
        (\ x_aaVzf
            -> case x_aaVzf of
                TabError y1_aaVzg -> Right y1_aaVzg
                _ -> Left x_aaVzf)
  _ExpectedGreaterThan
    = (Control.Lens.Prism.prism
          (\ (x1_aaVzi, x2_aaVzj)
            -> (ExpectedGreaterThan x1_aaVzi) x2_aaVzj))
        (\ x_aaVzk
            -> case x_aaVzk of
                ExpectedGreaterThan y1_aaVzl y2_aaVzm -> Right (y1_aaVzl, y2_aaVzm)
                _ -> Left x_aaVzk)
  _ExpectedEqualTo
    = (Control.Lens.Prism.prism
          (\ (x1_aaVzo, x2_aaVzp) -> (ExpectedEqualTo x1_aaVzo) x2_aaVzp))
        (\ x_aaVzq
            -> case x_aaVzq of
                ExpectedEqualTo y1_aaVzr y2_aaVzs -> Right (y1_aaVzr, y2_aaVzs)
                _ -> Left x_aaVzq)
