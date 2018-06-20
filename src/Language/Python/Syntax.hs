{-# language DataKinds #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# language OverloadedLists #-}
module Language.Python.Syntax where

import Data.Function ((&))
import Control.Lens.Getter ((^.))
import Control.Lens.Iso (from)
import Control.Lens.Setter ((.~), over)
import Data.List.NonEmpty (NonEmpty)

import Language.Python.Internal.Optics
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
  Fundef (Indents [] ()) ()
    [Space]
    name
    []
    (listToCommaSep params)
    []
    []
    LF
    (toBlock block)

call_ :: Expr '[] () -> [Arg '[] ()] -> Expr '[] ()
call_ expr args = Call () expr [] (listToCommaSep args) []

return_ :: Expr '[] () -> Statement '[] ()
return_ e = SmallStatements (Indents [] ()) (Return () [Space] e) [] Nothing (Just LF)

expr_ :: Expr '[] () -> Statement '[] ()
expr_ e = SmallStatements (Indents [] ()) (Expr () e) [] Nothing (Just LF)

list_ :: [Expr '[] ()] -> Expr '[] ()
list_ es = List () [] (listToCommaSep1' es) []

is_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
is_ a = BinOp () (a & trailingWhitespace .~ [Space]) (Is () [Space])
infixl 1 `is_`

(.==) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.==) a = BinOp () (a & trailingWhitespace .~ [Space]) (Equals () [Space])
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
(./) a = BinOp () (a & trailingWhitespace .~ [Space]) (Divide () [Space])
infixl 7 ./

(.//) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.//) = undefined
infixl 7 .//

(.%) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.%) = undefined
infixl 7 .%

(.**) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.**) a = BinOp () (a & trailingWhitespace .~ [Space]) (Exp () [Space])
infixr 8 .**

(/>) :: Expr '[] () -> Ident '[] () -> Expr '[] ()
(/>) a b = Deref () a [] b
infixl 9 />

neg :: Expr '[] () -> Expr '[] ()
neg = negate

toBlock :: NonEmpty (Statement '[] ()) -> Block '[] ()
toBlock sts =
  Block $
  Right .
  over (_Indents.indentsValue) (replicate 4 Space ^. from indentWhitespaces :) <$>
  sts

if_ :: Expr '[] () -> NonEmpty (Statement '[] ()) -> Statement '[] ()
if_ e sts =
  CompoundStatement $
  If (Indents [] ()) () [Space] e [] LF
    (toBlock sts)
    Nothing

while_ :: Expr '[] () -> NonEmpty (Statement '[] ()) -> Statement '[] ()
while_ e sts =
  CompoundStatement $
  While (Indents [] ()) () [Space] e
    [] LF
    (toBlock sts)

ifElse_ :: Expr '[] () -> NonEmpty (Statement '[] ()) -> NonEmpty (Statement '[] ()) -> Statement '[] ()
ifElse_ e sts sts' =
  CompoundStatement $
  If (Indents [] ()) () [Space] e [] LF
    (toBlock sts)
    (Just (Indents [] (), [], [], LF, toBlock sts'))

var_ :: String -> Expr '[] ()
var_ s = Ident () (MkIdent () s [])

none_ :: Expr '[] ()
none_ = None () []

pass_ :: Statement '[] ()
pass_ = SmallStatements (Indents [] ()) (Pass ()) [] Nothing (Just LF)

break_ :: Statement '[] ()
break_ = SmallStatements (Indents [] ()) (Break ()) [] Nothing (Just LF)

true_ :: Expr '[] ()
true_ = Bool () True []

false_ :: Expr '[] ()
false_ = Bool () False []

and_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
and_ a = BinOp () (a & trailingWhitespace .~ [Space]) (BoolAnd () [Space])

or_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
or_ a = BinOp () (a & trailingWhitespace .~ [Space]) (BoolOr () [Space])

str_ :: String -> Expr '[] ()
str_ s = String () Nothing DoubleQuote ShortString s []

longStr_ :: String -> Expr '[] ()
longStr_ s = String () Nothing DoubleQuote LongString s []

(.=) :: Expr '[] () -> Expr '[] () -> Statement '[] ()
(.=) a b =
  SmallStatements
    (Indents [] ())
    (Assign () (a & trailingWhitespace .~ [Space]) [Space] b) [] Nothing (Just LF)

forElse_
  :: Expr '[] ()
  -> Expr '[] ()
  -> NonEmpty (Statement '[] ())
  -> NonEmpty (Statement '[] ())
  -> Statement '[] ()
forElse_ val vals block els =
  CompoundStatement $
  For (Indents [] ()) () [Space] (val & trailingWhitespace .~ [Space]) [Space] vals [] LF
    (toBlock block)
    (Just (Indents [] (), [], [], LF, toBlock els))

for_ :: Expr '[] () -> Expr '[] () -> NonEmpty (Statement '[] ()) -> Statement '[] ()
for_ val vals block =
  CompoundStatement $
  For (Indents [] ()) () [Space] (val & trailingWhitespace .~ [Space]) [Space] vals [] LF
    (toBlock block)
    Nothing
