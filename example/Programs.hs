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
append_to =
  CompoundStatement $
  Fundef ()
    [Space]
    "append_to"
    []
    ( CommaSepMany (PositionalParam () "element") [] [Space] $
      CommaSepOne (KeywordParam () "to" [] [] (List () [] CommaSepNone []))
    )
    []
    []
    LF
    (Block
     [ ( ()
       , replicate 4 Space
       , SmallStatements
         (Expr () $
          Call ()
            (Deref () (Ident () "to") [] [] "append") []
            (CommaSepOne $ PositionalArg () (Ident () "element")))
         []
         Nothing
         LF
       )
     , ( ()
       , replicate 4 Space
       , SmallStatements (Return () [Space] (Ident () "to")) [] Nothing LF
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

everything =
  Module
    [ Right append_to
    , Left ([], LF)
    , Right append_to'
    , Left ([], LF)
    , Right fact_tr
    , Left ([], LF)
    , Right spin
    , Left ([], LF)
    , Right yes
    ]
