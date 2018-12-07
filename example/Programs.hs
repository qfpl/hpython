{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Programs where

import Control.Lens.Getter ((^.))
import Control.Lens.Iso (from)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))

import Language.Python.DSL

import Language.Python.Syntax.Module (Module(..))
import Language.Python.Syntax.CommaSep (Comma(..), CommaSep(..), CommaSep1'(..))
import Language.Python.Syntax.Expr (Arg(..), Expr(..), Param(..))
import Language.Python.Syntax.Punctuation (Colon(..))
import Language.Python.Syntax.Statement (Block(..), CompoundStatement(..), SmallStatement(..), SimpleStatement(..), Statement(..), Suite(..))
import Language.Python.Syntax.Whitespace (Indents(..), Newline(..), Whitespace(..), indentWhitespaces)

-- |
-- @
-- def append_to(element, to=[]):
--   to.append(element)
--   return to
-- @
--
-- Written without the DSL (not recommended!)
append_to :: Raw Statement
append_to =
  CompoundStatement $
  Fundef () [] (Indents [] ())
    Nothing
    (Space :| [])
    "append_to"
    []
    ( CommaSepMany (PositionalParam () "element" Nothing) (Comma [Space]) $
      CommaSepOne (KeywordParam () "to" Nothing [] (List () [] Nothing []))
    )
    []
    Nothing
    (SuiteMany () (Colon []) Nothing LF $
     Block []
     ( SmallStatement
         (Indents [replicate 4 Space ^. from indentWhitespaces] ())
         (MkSmallStatement
          (Expr () $
           Call ()
             (Deref () (Ident "to") [] "append")
             []
             (Just $ CommaSepOne1' (PositionalArg () (Ident "element")) Nothing)
             [])
          []
          Nothing
          Nothing
          (Just LF))
     )
     [ Right $
         SmallStatement
           (Indents [replicate 4 Space ^. from indentWhitespaces] ())
           (MkSmallStatement
            (Return () [Space] (Just $ Ident "to"))
            []
            Nothing
            Nothing
            (Just LF))
     ])

-- |
-- @
-- def append_to(element, to=[]):
--   to.append(element)
--   return to
-- @
--
-- Written with the DSL
append_to' :: Raw Statement
append_to' =
  def_ "append_to" [ p_ "element", k_ "to" (list_ []) ]
    [ line_ $ call_ ("to" /> "append") [ "element" ]
    , line_ $ return_ "to"
    ]

-- |
-- @
-- def fact(n)
--   def go(n, acc)
--     if n == 0:
--       return acc
--     else:
--       go(n-1, n*acc)
--   return go(n, 1)
-- @
fact_tr :: Raw Statement
fact_tr =
  def_ "fact" [p_ "n"]
  [ line_ $
    def_ "go" [p_ "n", p_ "acc"]
      [ line_ $
        if_ ("n" .== 0)
          [line_ $ return_ (var_ "acc")] &
        else_
          [line_ . return_ $ call_ "go" [p_ $ "n" .- 1, p_ $ "n" .* "acc"]]
      ]
  , line_ . return_ $ call_ "go" [p_ "n", p_ 1]
  ]

-- |
-- @
-- def spin():
--   spin()
-- @
spin :: Raw Statement
spin = def_ "spin" [] [line_ $ call_ "spin" []]

-- |
-- @
-- def yes()
--   print("yes")
--   yes()
-- @
yes :: Raw Statement
yes =
  def_ "yes" []
  [ line_ $ call_ "print" [p_ $ str_ "yes"]
  , line_ $ call_ "yes" []
  ]

everything :: Raw Module
everything =
  module_
  [ line_ append_to
  , blank_

  , line_ append_to'
  , blank_

  , line_ fact_tr
  , blank_

  , line_ spin
  , blank_

  , line_ yes
  ]
