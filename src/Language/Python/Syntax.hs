{-# language DataKinds #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# language OverloadedLists #-}
module Language.Python.Syntax where

import Data.List.NonEmpty (NonEmpty)

import Language.Python.Internal.Syntax

class HasPositional p v | p -> v where
  p_ :: v -> p

class HasKeyword p where
  k_ :: Ident '[] () -> Expr '[] () -> p

instance HasPositional (Param '[] ()) (Ident '[] ()) where; p_ = PositionalParam ()
instance HasKeyword (Param '[] ()) where; k_ a = KeywordParam () a [] []
instance HasPositional (Arg '[] ()) (Expr '[] ()) where; p_ = PositionalArg ()
instance HasKeyword (Arg '[] ()) where; k_ a = KeywordArg () a [] []

def_ :: Ident '[] () -> [Param '[] ()] -> NonEmpty (Statement '[] ()) -> Statement '[] ()
def_ name params block =
  CompoundStatement $
  Fundef ()
    [Space]
    name
    []
    (listToCommaSep params)
    []
    []
    LF
    (Block $ (,,) () [Space, Space, Space, Space] <$> block)

call_ :: Expr '[] () -> [Arg '[] ()] -> Expr '[] ()
call_ expr args = Call () expr [] (listToCommaSep args)

return_ :: Expr '[] () -> Statement '[] ()
return_ e = SmallStatements (Return () [Space] e) [] Nothing LF

expr_ :: Expr '[] () -> Statement '[] ()
expr_ e = SmallStatements (Expr () e) [] Nothing LF

list_ :: [Expr '[] ()] -> Expr '[] ()
list_ es = List () [] (listToCommaSep es) []

is_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
is_ a = BinOp () a [Space] (Is ()) [Space]
infixl 1 `is_`

(.==) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.==) a = BinOp () a [Space] (Equals ()) [Space]
infixl 1 .==

(.|) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.|) = undefined
infixl 2 .|

(.^) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.^) = undefined
infixl 3 .^

(.&) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.&) = undefined
infixl 4 .&

(.<<) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.<<) = undefined
infixl 5 .<<

(.>>) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.>>) = undefined
infixl 5 .>>

(.+) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.+) = (+)
infixl 6 .+

(.-) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.-) = (-)
infixl 6 .-

(.*) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.*) = (*)
infixl 7 .*

(.@) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.@) = undefined
infixl 7 .@

(./) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(./) a = BinOp () a [Space] (Divide ()) [Space]
infixl 7 ./

(.//) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.//) = undefined
infixl 7 .//

(.%) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.%) = undefined
infixl 7 .%

(.**) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.**) a = BinOp () a [Space] (Exp ()) [Space]
infixr 8 .**

(/>) :: Expr '[] () -> Ident '[] () -> Expr '[] ()
(/>) a = Deref () a [] []
infixl 9 />

neg :: Expr '[] () -> Expr '[] ()
neg = negate

if_ :: Expr '[] () -> NonEmpty (Statement '[] ()) -> Statement '[] ()
if_ e sts =
  CompoundStatement $
  If () [Space] e [] [] LF
    (Block $ (,,) () [Space, Space, Space, Space] <$> sts)
    Nothing

while_ :: Expr '[] () -> NonEmpty (Statement '[] ()) -> Statement '[] ()
while_ e sts =
  CompoundStatement $
  While () [Space] e
    [] [] LF
    (Block $ (,,) () [Space, Space, Space, Space] <$> sts)

ifElse_ :: Expr '[] () -> NonEmpty (Statement '[] ()) -> NonEmpty (Statement '[] ()) -> Statement '[] ()
ifElse_ e sts sts' =
  CompoundStatement $
  If () [Space] e [] [] LF
    (Block $ (,,) () [Space, Space, Space, Space] <$> sts)
    (Just ([], [], LF, Block $ (,,) () [Space, Space, Space, Space] <$> sts'))

var_ :: String -> Expr '[] ()
var_ = Ident () . MkIdent ()

none_ :: Expr '[] ()
none_ = None ()

pass_ :: Statement '[] ()
pass_ = SmallStatements (Pass ()) [] Nothing LF

break_ :: Statement '[] ()
break_ = SmallStatements (Break ()) [] Nothing LF

true_ :: Expr '[] ()
true_ = Bool () True

false_ :: Expr '[] ()
false_ = Bool () False

and_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
and_ a = BinOp () a [Space] (BoolAnd ()) [Space]

or_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
or_ a = BinOp () a [Space] (BoolOr ()) [Space]

str_ :: String -> Expr '[] ()
str_ = String () ShortDouble

longStr_ :: String -> Expr '[] ()
longStr_ = String () LongDouble

(.=) :: Expr '[] () -> Expr '[] () -> Statement '[] ()
(.=) a b = SmallStatements (Assign () a [Space] [Space] b) [] Nothing LF
