{-# language OverloadedLists, OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Programs where

import Control.Lens.Getter ((^.))
import Control.Lens.Iso (from)
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
  Fundef () [] (Indents [] ())
    Nothing
    [Space]
    "append_to"
    []
    ( CommaSepMany (PositionalParam () "element" Nothing) [Space] $
      CommaSepOne (KeywordParam () "to" Nothing [] (List () [] Nothing []))
    )
    []
    Nothing
    (SuiteMany () [] (LF Nothing) $
     Block
     [ Right $
       SmallStatements
         (Indents [replicate 4 Space ^. from indentWhitespaces] ())
         (Expr () $
          Call ()
            (Deref () (Ident "to") [] "append")
            []
            (Just $ CommaSepOne1' (PositionalArg () (Ident "element")) Nothing)
            [])
         []
         Nothing
         (Right $ LF Nothing)
     , Right $
       SmallStatements
         (Indents [replicate 4 Space ^. from indentWhitespaces] ())
         (Return () [Space] (Just $ Ident "to"))
         []
         Nothing
         (Right $ LF Nothing)
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
fact_tr =
  def_ "fact" [p_ "n"]
  [ line_ $
    def_ "go" [p_ "n", p_ "acc"]
      [ line_ $
        ifElse_ ("n" .== 0)
          [line_ $ return_ "acc"]
          [line_ . return_ $ call_ "go" [p_ $ "n" .- 1, p_ $ "n" .* "acc"]]
      ]
  , line_ . return_ $ call_ "go" [p_ "n", p_ 1]
  ]

-- |
-- @
-- def spin():
--   spin()
-- @
spin = def_ "spin" [] [line_ $ call_ "spin" []]

-- |
-- @
-- def yes()
--   print("yes")
--   yes()
-- @
yes =
  def_ "yes" []
  [ line_ $ call_ "print" [p_ $ str_ "yes"]
  , line_ $ call_ "yes" []
  ]

everything =
  Module
    [ Right append_to
    , Left (Indents [] (), Nothing, Just $ LF Nothing)
    , Right append_to'
    , Left (Indents [] (), Nothing, Just $ LF Nothing)
    , Right fact_tr
    , Left (Indents [] (), Nothing, Just $ LF Nothing)
    , Right spin
    , Left (Indents [] (), Nothing, Just $ LF Nothing)
    , Right yes
    ]
