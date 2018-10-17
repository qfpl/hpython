{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Programs where

import Control.Lens.Getter ((^.))
import Control.Lens.Iso (from)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import Language.Python.Internal.Syntax
import Language.Python.Syntax

-- |
-- @
-- def append_to(element, to=[]):
--   to.append(element)
--   return to
-- @
--
-- Written without the DSL
append_to :: Raw Statement
append_to =
  CompoundStatement $
  Fundef () [] (Indents [] ())
    Nothing
    (Space :| [])
    "append_to"
    []
    ( CommaSepMany (PositionalParam () "element" Nothing) [Space] $
      CommaSepOne (KeywordParam () "to" Nothing [] (List () [] Nothing []))
    )
    []
    Nothing
    (SuiteMany () [] Nothing LF $
     Block []
     ( SimpleStatement
         (Indents [replicate 4 Space ^. from indentWhitespaces] ())
         (MkSimpleStatement
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
         SimpleStatement
           (Indents [replicate 4 Space ^. from indentWhitespaces] ())
           (MkSimpleStatement
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
        if_ ("n" .== 0) ([line_ $ return_ (var_ "acc")] :: [Raw Line]) &
        else_ [line_ . return_ $ call_ "go" [p_ $ "n" .- 1, p_ $ "n" .* "acc"]]
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
  ModuleStatement append_to $
  ModuleBlank (Blank () [] Nothing) LF $

  ModuleStatement append_to' $
  ModuleBlank (Blank () [] Nothing) LF $

  ModuleStatement fact_tr $
  ModuleBlank (Blank () [] Nothing) LF $

  ModuleStatement spin $
  ModuleBlank (Blank () [] Nothing) LF $

  ModuleStatement yes ModuleEmpty
