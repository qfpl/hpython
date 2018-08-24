{-# language DataKinds #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# language OverloadedLists #-}
{-# language RecordWildCards #-}
{-# language KindSignatures, TemplateHaskell #-}
module Language.Python.Syntax
  ( Raw
  , Statement
  , Expr
    -- * Parameters and arguments
  , HasPositional(..)
  , HasKeyword(..)
    -- ** Parameters
  , HasParameters(..)
    -- * Decorators
  , HasDecorators(..)
  , decorated_
    -- * Statements
    -- ** Function definitions
  , Fundef(..)
  , mkFundef
  , fundef
  , def_
    -- *** Lenses
  , fdAnn
  , fdDecorators
  , fdIndents
  , fdAsync
  , fdDefSpaces
  , fdName
  , fdLeftParenSpaces
  , fdParameters
  , fdRightParenSpaces
  , fdReturnType
  , fdBody
    -- ** Assignment
  , (.=)
    -- ** Flow control
  , if_
  , ifElse_
  , return_
    -- * Expressions
  , expr_
  , var_
  , call_
    -- * Literals
  , list_
  , none_
  , str_
    -- ** Dereferencing
  , (/>)
    -- ** Binary operators
  , is_
  , (.*)
  , (.-)
  , (.==)
  )
where

import Data.Function ((&))
import Control.Lens.Getter ((^.))
import Control.Lens.Iso (from)
import Control.Lens.Lens (Lens')
import Control.Lens.Setter ((.~), over)
import Data.List.NonEmpty (NonEmpty)

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax hiding (Fundef)
import Language.Python.Syntax.Types
import qualified Language.Python.Internal.Syntax as AST (CompoundStatement(Fundef))

type Raw f = f '[] ()

class HasPositional p v | p -> v where
  p_ :: v -> p

class HasKeyword p where
  k_ :: Ident '[] () -> Raw Expr -> p

instance HasPositional (Param '[] ()) (Ident '[] ()) where
  p_ i = PositionalParam () i Nothing
instance HasKeyword (Param '[] ()) where
  k_ a = KeywordParam () a Nothing []
instance HasPositional (Arg '[] ()) (Raw Expr) where; p_ = PositionalArg ()
instance HasKeyword (Arg '[] ()) where; k_ a = KeywordArg () a []

class HasParameters s where
  setParameters :: [Raw Param] -> Raw s -> Raw s
  parameters :: Lens' (Raw s) (CommaSep (Raw Param))

class HasDecorators s where
  decorators :: Lens' (Raw s) [Raw Decorator]

decorated_ :: HasDecorators s => [Raw Decorator] -> Raw s -> Raw s
decorated_ ds s = s & decorators .~ ds

instance HasDecorators Fundef where
  decorators = fdDecorators

instance HasParameters Fundef where
  setParameters p = fdParameters .~ listToCommaSep p
  parameters = fdParameters

mkFundef :: Ident '[] () -> NonEmpty (Raw Statement) -> Raw Fundef
mkFundef name body =
  Fundef
  { _fdAnn = ()
  , _fdDecorators = []
  , _fdIndents = Indents [] ()
  , _fdAsync = Nothing
  , _fdDefSpaces = pure Space
  , _fdName = name
  , _fdLeftParenSpaces = []
  , _fdParameters = CommaSepNone
  , _fdRightParenSpaces = []
  , _fdReturnType = Nothing
  , _fdBody = SuiteMany () [] (LF Nothing) $ toBlock body
  }

fundef :: Raw Fundef -> Raw Statement
fundef Fundef{..} =
  CompoundStatement $
  AST.Fundef ()
    _fdDecorators
    _fdIndents
    _fdAsync
    _fdDefSpaces
    _fdName
    _fdLeftParenSpaces
    _fdParameters
    _fdRightParenSpaces
    _fdReturnType
    _fdBody

def_ :: Ident '[] () -> [Param '[] ()] -> NonEmpty (Raw Statement) -> Raw Statement
def_ name params body =
  fundef $ (mkFundef name body) { _fdParameters = listToCommaSep params }

call_ :: Raw Expr -> [Arg '[] ()] -> Raw Expr
call_ expr args =
  Call ()
    expr
    []
    (case args of
      [] -> Nothing
      a:as -> Just $ (a, zip (repeat [Space]) as, Nothing) ^. _CommaSep1')
    []

return_ :: Raw Expr -> Raw Statement
return_ e =
  SmallStatements (Indents [] ()) (Return () [Space] $ Just e) [] Nothing (Right (LF Nothing))

expr_ :: Raw Expr -> Raw Statement
expr_ e = SmallStatements (Indents [] ()) (Expr () e) [] Nothing (Right (LF Nothing))

list_ :: [Raw Expr] -> Raw Expr
list_ es = List () [] (listToCommaSep1' $ ListItem () <$> es) []

is_ :: Raw Expr -> Raw Expr -> Raw Expr
is_ a = BinOp () (a & trailingWhitespace .~ [Space]) (Is () [Space])
infixl 1 `is_`

(.==) :: Raw Expr -> Raw Expr -> Raw Expr
(.==) a = BinOp () (a & trailingWhitespace .~ [Space]) (Equals () [Space])
infixl 1 .==

(.|) :: Raw Expr -> Raw Expr -> Raw Expr
(.|) = undefined
infixl 2 .|

(.^) :: Raw Expr -> Raw Expr -> Raw Expr
(.^) = undefined
infixl 3 .^

(.&) :: Raw Expr -> Raw Expr -> Raw Expr 
(.&) = undefined
infixl 4 .&

(.<<) :: Raw Expr -> Raw Expr -> Raw Expr 
(.<<) = undefined
infixl 5 .<<

(.>>) :: Raw Expr -> Raw Expr -> Raw Expr 
(.>>) = undefined
infixl 5 .>>

(.+) :: Raw Expr -> Raw Expr -> Raw Expr 
(.+) = (+)
infixl 6 .+

(.-) :: Raw Expr -> Raw Expr -> Raw Expr 
(.-) = (-)
infixl 6 .-

(.*) :: Raw Expr -> Raw Expr -> Raw Expr 
(.*) = (*)
infixl 7 .*

(.@) :: Raw Expr -> Raw Expr -> Raw Expr
(.@) = undefined
infixl 7 .@

(./) :: Raw Expr -> Raw Expr -> Raw Expr
(./) a = BinOp () (a & trailingWhitespace .~ [Space]) (Divide () [Space])
infixl 7 ./

(.//) :: Raw Expr -> Raw Expr -> Raw Expr
(.//) a = BinOp () (a & trailingWhitespace .~ [Space]) (FloorDivide () [Space])
infixl 7 .//

(.%) :: Raw Expr -> Raw Expr -> Raw Expr
(.%) a = BinOp () (a & trailingWhitespace .~ [Space]) (Percent () [Space])
infixl 7 .%

(.**) :: Raw Expr -> Raw Expr -> Raw Expr
(.**) a = BinOp () (a & trailingWhitespace .~ [Space]) (Exp () [Space])
infixr 8 .**

(/>) :: Raw Expr -> Ident '[] () -> Raw Expr
(/>) a b = Deref () a [] b
infixl 9 />

neg :: Raw Expr -> Raw Expr
neg = negate

toBlock :: NonEmpty (Raw Statement) -> Block '[] ()
toBlock sts =
  Block $
  Right .
  over (_Indents.indentsValue) (replicate 4 Space ^. from indentWhitespaces :) <$>
  sts

while_ :: Raw Expr -> NonEmpty (Raw Statement) -> Raw Statement
while_ e sts =
  CompoundStatement $
  While () (Indents [] ()) [Space] e
    (SuiteMany () [] (LF Nothing) $ toBlock sts)

ifElifsElse_
  :: Raw Expr
  -> NonEmpty (Raw Statement)
  -> [(Raw Expr, NonEmpty (Raw Statement))]
  -> NonEmpty (Raw Statement)
  -> Raw Statement
ifElifsElse_ e sts elifs sts' =
  CompoundStatement $
  If () (Indents [] ()) [Space] e
    (SuiteMany () [] (LF Nothing) $ toBlock sts)
    ((\(a, b) -> (Indents [] (), [Space], a, SuiteMany () [] (LF Nothing) $ toBlock b)) <$> elifs)
    (Just (Indents [] (), [], SuiteMany () [] (LF Nothing) $ toBlock sts'))

if_ :: Raw Expr -> NonEmpty (Raw Statement) -> Raw Statement
if_ e sts =
  CompoundStatement $
  If () (Indents [] ()) [Space] e
    (SuiteMany () [] (LF Nothing) $ toBlock sts)
    []
    Nothing

ifElse_
  :: Raw Expr
  -> NonEmpty (Raw Statement)
  -> NonEmpty (Raw Statement)
  -> Raw Statement
ifElse_ e sts = ifElifsElse_ e sts []

var_ :: String -> Raw Expr
var_ s = Ident () (MkIdent () s [])

none_ :: Raw Expr
none_ = None () []

pass_ :: Raw Statement
pass_ = SmallStatements (Indents [] ()) (Pass () []) [] Nothing (Right (LF Nothing))

break_ :: Raw Statement
break_ = SmallStatements (Indents [] ()) (Break () []) [] Nothing (Right (LF Nothing))

true_ :: Raw Expr
true_ = Bool () True []

false_ :: Raw Expr
false_ = Bool () False []

and_ :: Raw Expr -> Raw Expr -> Raw Expr
and_ a = BinOp () (a & trailingWhitespace .~ [Space]) (BoolAnd () [Space])

or_ :: Raw Expr -> Raw Expr -> Raw Expr
or_ a = BinOp () (a & trailingWhitespace .~ [Space]) (BoolOr () [Space])

str_ :: String -> Raw Expr
str_ s =
  String () . pure $
  StringLiteral () Nothing DoubleQuote ShortString (Char_lit <$> s) []

longStr_ :: String -> Raw Expr
longStr_ s =
  String () . pure $
  StringLiteral () Nothing DoubleQuote LongString (Char_lit <$> s) []

(.=) :: Raw Expr -> Raw Expr -> Raw Statement
(.=) a b =
  SmallStatements
    (Indents [] ())
    (Assign () (a & trailingWhitespace .~ [Space]) $ pure ([Space], b))
    []
    Nothing
    (Right (LF Nothing))

forElse_
  :: Raw Expr
  -> Raw Expr
  -> NonEmpty (Raw Statement)
  -> NonEmpty (Raw Statement)
  -> Raw Statement
forElse_ val vals block els =
  CompoundStatement $
  For () (Indents [] ()) Nothing [Space] (val & trailingWhitespace .~ [Space]) [Space] vals
    (SuiteMany () [] (LF Nothing) $ toBlock block)
    (Just (Indents [] (), [], SuiteMany () [] (LF Nothing) $ toBlock els))

for_ :: Raw Expr -> Raw Expr -> NonEmpty (Raw Statement) -> Raw Statement
for_ val vals block =
  CompoundStatement $
  For () (Indents [] ()) Nothing [Space] (val & trailingWhitespace .~ [Space]) [Space] vals
    (SuiteMany () [] (LF Nothing) $ toBlock block)
    Nothing
