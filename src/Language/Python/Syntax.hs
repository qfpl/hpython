{-# language DataKinds #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# language OverloadedLists #-}
module Language.Python.Syntax where

import Data.Function ((&))
import Control.Lens.Setter ((.~))
import Data.List.NonEmpty (NonEmpty)

import Language.Python.Internal.Syntax

class HasPositional p v | p -> v where
  p_ :: v -> p

class HasKeyword p where
  k_ :: Ident '[] () -> Expr '[] () -> p

instance HasPositional (Param '[] ()) (Ident '[] ()) where; p_ = PositionalParam ()
instance HasKeyword (Param '[] ()) where; k_ a = KeywordParam () a []
instance HasPositional (Arg '[] ()) (Expr '[] ()) where; p_ = PositionalArg ()
instance HasKeyword (Arg '[] ()) where; k_ a = KeywordArg () a []

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
    (Block $ (,,) () [Space, Space, Space, Space] . Right <$> block)

call_ :: Expr '[] () -> [Arg '[] ()] -> Expr '[] ()
call_ expr args = Call () expr [] (listToCommaSep args) []

return_ :: Expr '[] () -> Statement '[] ()
return_ e = SmallStatements (Return () [Space] e) [] Nothing LF

expr_ :: Expr '[] () -> Statement '[] ()
expr_ e = SmallStatements (Expr () e) [] Nothing LF

list_ :: [Expr '[] ()] -> Expr '[] ()
list_ es = List () [] (listToCommaSep es) []

is_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
is_ a = BinOp () (a & whitespaceAfter .~ [Space]) (Is () [Space])
infixl 1 `is_`

(.==) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.==) a = BinOp () (a & whitespaceAfter .~ [Space]) (Equals () [Space])
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
(./) a = BinOp () (a & whitespaceAfter .~ [Space]) (Divide () [Space])
infixl 7 ./

(.//) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.//) = undefined
infixl 7 .//

(.%) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.%) = undefined
infixl 7 .%

(.**) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.**) a = BinOp () (a & whitespaceAfter .~ [Space]) (Exp () [Space])
infixr 8 .**

(/>) :: Expr '[] () -> Ident '[] () -> Expr '[] ()
(/>) a b = Deref () a [] b
infixl 9 />

neg :: Expr '[] () -> Expr '[] ()
neg = negate

toBlock :: NonEmpty (Statement vs ()) -> Block vs ()
toBlock sts = Block $ (,,) () [Space, Space, Space, Space] . Right <$> sts

if_ :: Expr '[] () -> NonEmpty (Statement '[] ()) -> Statement '[] ()
if_ e sts =
  CompoundStatement $
  If () [Space] e [] [] LF
    (toBlock sts)
    Nothing

while_ :: Expr '[] () -> NonEmpty (Statement '[] ()) -> Statement '[] ()
while_ e sts =
  CompoundStatement $
  While () [Space] e
    [] [] LF
    (toBlock sts)

ifElse_ :: Expr '[] () -> NonEmpty (Statement '[] ()) -> NonEmpty (Statement '[] ()) -> Statement '[] ()
ifElse_ e sts sts' =
  CompoundStatement $
  If () [Space] e [] [] LF
    (toBlock sts)
    (Just ([], [], LF, toBlock sts'))

var_ :: String -> Expr '[] ()
var_ s = Ident () (MkIdent () s [])

none_ :: Expr '[] ()
none_ = None () []

pass_ :: Statement '[] ()
pass_ = SmallStatements (Pass ()) [] Nothing LF

break_ :: Statement '[] ()
break_ = SmallStatements (Break ()) [] Nothing LF

true_ :: Expr '[] ()
true_ = Bool () True []

false_ :: Expr '[] ()
false_ = Bool () False []

and_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
and_ a = BinOp () (a & whitespaceAfter .~ [Space]) (BoolAnd () [Space])

or_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
or_ a = BinOp () (a & whitespaceAfter .~ [Space]) (BoolOr () [Space])

str_ :: String -> Expr '[] ()
str_ s = String () Nothing ShortDouble s []

longStr_ :: String -> Expr '[] ()
longStr_ s = String () Nothing LongDouble s []

(.=) :: Expr '[] () -> Expr '[] () -> Statement '[] ()
(.=) a b = SmallStatements (Assign () a [Space] [Space] b) [] Nothing LF

forElse_
  :: Expr '[] ()
  -> Expr '[] ()
  -> NonEmpty (Statement '[] ())
  -> NonEmpty (Statement '[] ())
  -> Statement '[] ()
forElse_ val vals block els =
  CompoundStatement $
  For () [Space] (val & whitespaceAfter .~ [Space]) [Space] vals [] LF
    (toBlock block)
    (Just ([], [], LF, toBlock els))

for_ :: Expr '[] () -> Expr '[] () -> NonEmpty (Statement '[] ()) -> Statement '[] ()
for_ val vals block =
  CompoundStatement $
  For () [Space] (val & whitespaceAfter .~ [Space]) [Space] vals [] LF
    (toBlock block)
    Nothing
