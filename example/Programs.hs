{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Programs where

import Control.Lens.Getter ((^.))
import Control.Lens.Iso (from)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))

import Language.Python.DSL
import Language.Python.Syntax


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
  Fundef (Ann ()) [] (Indents [] (Ann ()))
    Nothing
    (Space :| [])
    "append_to"
    []
    ( CommaSepMany (PositionalParam (Ann ()) "element" Nothing) (MkComma [Space]) $
      CommaSepOne (KeywordParam (Ann ()) "to" Nothing [] (List (Ann ()) [] Nothing []))
    )
    []
    Nothing
    (SuiteMany (Ann ()) (MkColon []) Nothing LF $
     Block []
     ( SmallStatement
         (Indents [replicate 4 Space ^. from indentWhitespaces] (Ann ()))
         (MkSmallStatement
          (Expr (Ann ()) $
           Call (Ann ())
             (Deref (Ann ()) (Ident (Ann ()) "to") [] "append")
             []
             (Just $ CommaSepOne1' (PositionalArg (Ann ()) (Ident (Ann ()) "element")) Nothing)
             [])
          []
          Nothing
          Nothing
          (Just LF))
     )
     [ Right $
         SmallStatement
           (Indents [replicate 4 Space ^. from indentWhitespaces] (Ann ()))
           (MkSmallStatement
            (Return (Ann ()) [Space] (Just $ Ident (Ann ()) "to"))
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
append_to' :: Raw Fundef
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
fact_tr :: Raw Fundef
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
spin :: Raw Fundef
spin = def_ "spin" [] [line_ $ call_ "spin" []]

-- |
-- @
-- def yes()
--   print("yes")
--   yes()
-- @
yes :: Raw Fundef
yes =
  def_ "yes" []
  [ line_ $ call_ "print" [p_ $ str_ "yes"]
  , line_ $ call_ "yes" []
  ]

counter :: Raw ClassDef
counter =
  class_ "Counter" []
  [ line_ $
    def_ "__init__" ["self"]
      [line_ ("self" /> "x" .= 0)]

  , blank_

  , line_ $
    def_ "incr" ["self"]
      [line_ ("self" /> "x" .+= 1)]

  , blank_

  , line_ $
    def_ "reset" ["self"]
      [line_ ("self" /> "x" .= 0)]

  , blank_

  , line_ $
    def_ "get" ["self"]
      [line_ $ return_ ("self" /> "x")]
  ]

exceptions :: Raw Fundef
exceptions =
  def_ "exceptions" []
  [ line_ $
    tryE_ [line_ pass_] &
      except_ [line_ pass_]
  , blank_

  , line_ $
    tryE_ [line_ pass_] &
      exceptAs_ (var_ "a" `as_` id_ "b") [line_ pass_]
  , blank_

  , line_ $
    tryE_ [line_ pass_] &
      exceptAs_ (var_ "a" `as_` id_ "b") [line_ pass_] &
      finally_ [line_ pass_]
  , blank_

  , line_ $
    tryE_ [line_ pass_] &
      exceptAs_ (var_ "a" `as_` id_ "b") [line_ pass_] &
      else_ [line_ pass_] &
      finally_ [line_ pass_]
  , blank_

  , line_ $ tryF_ [line_ pass_] [line_ pass_]
  , blank_

  , line_ $ tryF_ [line_ pass_] & finally_ [line_ pass_]
  , blank_

  , line_ $
    tryF_ [line_ pass_] [line_ pass_] &
      exceptAs_ (var_ "a" `as_` id_ "b") [line_ pass_] &
      else_ [line_ pass_]
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
  , blank_

  , line_ counter
  , blank_

  , line_ exceptions
  ]
