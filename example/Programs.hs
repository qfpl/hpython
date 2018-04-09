{-# language OverloadedLists, OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Programs where

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
append_to a =
  CompoundStatement $
  Fundef a
    [Space]
    "append_to"
    []
    ( CommaSepMany (PositionalParam a "element") [] [Space] $
      CommaSepOne (KeywordParam a "to" [] [] (List a [] CommaSepNone []))
    )
    []
    []
    LF
    (Block
     [ ( a
       , replicate 4 Space
       , SmallStatements
         (Expr a $
          Call a
            (Deref a (Ident a "to") [] [] "append") []
            (CommaSepOne $ PositionalArg a (Ident a "element")))
         []
         Nothing
         LF
       )
     , ( a
       , replicate 4 Space
       , SmallStatements (Return a [Space] (Ident a "to")) [] Nothing LF
       )
     ])

-- |
-- @
-- def append_to(element, to=[]):
--   to.append(element)
--   return to
-- @
--
-- Written with the DSL
append_to' =
  def_ "append_to" [ p_ "element", k_ "to" (list_ []) ]
    [ expr_ $ call_ ("to" /> "append") [ "element" ]
    , return_ "to"
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
fact_tr =
  def_ "fact" [p_ "n"]
  [ def_ "go" [p_ "n", p_ "acc"]
    [ ifElse_ ("n" .== 0)
      [return_ "acc"]
      [return_ $ call_ "go" [p_ $ "n" .- 1, p_ $ "n" .* "acc"]]
    ]
  , return_ $ call_ "go" [p_ "n", p_ 1]
  ]

-- |
-- @
-- def spin():
--   spin()
-- @
spin = def_ "spin" [] [expr_ $ call_ "spin" []]

-- |
-- @
-- def yes()
--   print("yes")
--   yes()
-- @
yes =
  def_ "yes" []
  [ expr_ $ call_ "print" [p_ $ str_ "yes"]
  , expr_ $ call_ "yes" []
  ]
