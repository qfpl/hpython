{-# language DataKinds #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# language OverloadedLists #-}
{-# language LambdaCase #-}
module Language.Python.Syntax
  ( Raw
  , Statement
  , Expr
    -- * Parameters and arguments
    -- ** Parameters
  , HasParameters(..)
    -- ** Positional
  , HasPositional(..)
    -- ** Keyword
  , HasKeyword(..)
  , KeywordParam(..)
  , _KeywordParam
    -- *** Lenses
  , kpAnn 
  , kpName
  , kpType
  , kpEquals
  , kpExpr
    -- * Decorators
  , HasDecorators(..)
  , decorated_
    -- * Statements
    -- ** Block bodies
  , blank_
  , st_
  , Line(..)
  , HasBody(..)
  , modifyBody
    -- ** Function definitions
  , Fundef(..)
  , _Fundef
  , mkFundef
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
  , break_
  , for_
  , if_
  , ifElse_
  , pass_
  , return_
  , while_
    -- * Expressions
  , expr_
  , var_
  , call_
    -- * Literals
  , list_
  , none_
  , str_
  , true_
  , false_
    -- ** Dereferencing
  , (/>)
    -- ** Binary operators
  , is_
  , (.*)
  , (.-)
  , (.+)
  , (.==)
  )
where

import Data.Function ((&))
import Control.Lens.Getter ((^.))
import Control.Lens.Iso (from)
import Control.Lens.Lens (Lens')
import Control.Lens.Prism (_Right)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((.~), over)
import Control.Lens.Traversal (traverseOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax hiding (Fundef, While)
import Language.Python.Syntax.Types
import qualified Language.Python.Internal.Syntax as AST (CompoundStatement(While))

type Raw f = f '[] ()

blank_ :: Raw Line
blank_ = Line $ Left ([], LF Nothing)

st_ :: Raw Statement -> Raw Line
st_ = Line . Right

newtype Line v a = Line { unLine :: Either ([Whitespace], Newline) (Statement v a) }
  deriving (Eq, Show)

instance HasExprs Line where
  _Exprs f (Line a) = Line <$> (_Right._Exprs) f a

instance HasStatements Line where
  _Statements f (Line a) = Line <$> _Right f a

-- |
-- @forall ws s. id ~ getBody . flip (setBody ws) s@
--
-- @forall ws. let f x = setBody ws (getBody x) x in f ~ f . f@
class HasBody s where
  -- | Set the block body of some code, first indententing the block by a specified amount,
  -- then indenting it by the level of its parent
  --
  -- For example, if we wanted to set the body of @function_b@ to
  --
  -- @
  -- while True:
  --   pass
  -- @
  --
  -- in
  --
  -- @
  -- def function_a():
  --   def function_b():
  --     pass
  --     if True:
  --       pass
  --     else:
  --       pass
  -- @
  --
  -- then we would get
  --
  -- @
  -- def function_a():
  --   def function_b():
  --     while True:
  --       pass
  -- @
  setBody
    :: [Whitespace] -- ^ Indentation scheme for the new lines
    -> NonEmpty (Raw Line) -- ^ Lines to become the new body
    -> Raw s -- ^ Current code
    -> Raw s

  -- | View the block body of some code without the indentation from its outer context
  --
  -- For example, if we ran 'getBody' @function_b@ in this code:
  --
  -- @
  -- def function_a():
  --   def function_b():
  --     pass
  --     if True:
  --       pass
  --     else:
  --       pass
  -- @
  --
  -- then we would get
  --
  -- @
  -- pass
  -- if True:
  --   pass
  -- else:
  --   pass
  -- @
  getBody :: Raw s -> NonEmpty (Raw Line)
  body :: Lens' (Raw s) (Raw Suite)

doIndent :: [Whitespace] -> [Indent] -> [Indent]
doIndent ws a = ws ^. from indentWhitespaces : a

doDedent :: Indents a -> Indents a
doDedent i@(Indents [] _) = i
doDedent (Indents (a:b) c) = Indents b c

instance HasBody Fundef where
  body = fdBody

  setBody ws new fun =
    fun
    { _fdBody =
      over
        (_Indents.indentsValue)
        ((_fdIndents fun ^. indentsValue <>) . doIndent ws)
        (SuiteMany () [] (LF Nothing) . Block $ unLine <$> new)
    }

  getBody fun =
    (\case
        SuiteOne a b c d ->
          st_ (SmallStatements (Indents [] ()) c [] Nothing (Right d)) :| []
        SuiteMany a b c d ->
          Line <$> unBlock d) $
    fromMaybe
      (error "malformed indentation in function block")
      (traverseOf _Indents (fmap doDedent . subtractStart (_fdIndents fun)) (_fdBody fun))

-- | Modify the block body of some code
modifyBody
  :: HasBody s
  => [Whitespace] -- ^ New indentation scheme
  -> (NonEmpty (Raw Line) -> NonEmpty (Raw Line)) -- ^ Modification function
  -> Raw s -- ^ Existing code
  -> Raw s
modifyBody ws f fun = setBody ws (f $ getBody fun) fun

-- | Positional parameters/arguments
--
-- @
-- p_ :: 'Raw' 'Expr' -> 'Raw' 'Arg'
-- @
--
-- @
-- p_ :: 'Raw' 'Ident' -> 'Raw' 'Param'
-- @
class HasPositional p v | p -> v where
  p_ :: v -> p

-- | Keyword parameters/arguments
class HasKeyword p where
  k_ :: Raw Ident -> Raw Expr -> p

instance HasPositional (Raw Param) (Raw Ident) where
  p_ i = PositionalParam () i Nothing

instance HasKeyword (Raw Param) where
  k_ a = KeywordParam () a Nothing []

instance HasPositional (Raw Arg) (Raw Expr) where; p_ = PositionalArg ()

instance HasKeyword (Raw Arg) where; k_ a = KeywordArg () a []

class HasParameters s where
  setParameters :: [Raw Param] -> Raw s -> Raw s
  parameters :: Lens' (Raw s) (CommaSep (Raw Param))

class HasDecorators s where
  setDecorators :: [Raw Expr] -> Raw s -> Raw s
  getDecorators :: Raw s -> [Raw Expr]
  decorators :: Lens' (Raw s) [Raw Decorator]

decorated_ :: HasDecorators s => [Raw Expr] -> Raw s -> Raw s
decorated_ = setDecorators

instance HasDecorators Fundef where
  decorators = fdDecorators

  setDecorators new code =
    code
    { _fdDecorators = (\e -> Decorator () (_fdIndents code) [] e $ LF Nothing) <$> new
    }

  getDecorators code =
    (\(Decorator () _ _ e _) -> e) <$> _fdDecorators code

instance HasParameters Fundef where
  setParameters p = fdParameters .~ listToCommaSep p
  parameters = fdParameters

mkFundef :: Raw Ident -> NonEmpty (Raw Line) -> Raw Fundef
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

def_ :: Raw Ident -> [Raw Param] -> NonEmpty (Raw Line) -> Raw Statement
def_ name params body =
  _Fundef # (mkFundef name body) { _fdParameters = listToCommaSep params }

call_ :: Raw Expr -> [Raw Arg] -> Raw Expr
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

(/>) :: Raw Expr -> Raw Ident -> Raw Expr
(/>) a b = Deref () a [] b
infixl 9 />

neg :: Raw Expr -> Raw Expr
neg = negate

toBlock :: NonEmpty (Raw Line) -> Block '[] ()
toBlock = over (_Indents.indentsValue) (doIndent $ replicate 4 Space) . Block . fmap unLine

while_ :: Raw Expr -> NonEmpty (Raw Line) -> Raw Statement
while_ e sts =
  CompoundStatement $
  AST.While () (Indents [] ()) [Space] e
    (SuiteMany () [] (LF Nothing) $ toBlock sts)

ifElifsElse_
  :: Raw Expr
  -> NonEmpty (Raw Line)
  -> [(Raw Expr, NonEmpty (Raw Line))]
  -> NonEmpty (Raw Line)
  -> Raw Statement
ifElifsElse_ e sts elifs sts' =
  CompoundStatement $
  If () (Indents [] ()) [Space] e
    (SuiteMany () [] (LF Nothing) $ toBlock sts)
    ((\(a, b) -> (Indents [] (), [Space], a, SuiteMany () [] (LF Nothing) $ toBlock b)) <$> elifs)
    (Just (Indents [] (), [], SuiteMany () [] (LF Nothing) $ toBlock sts'))

if_ :: Raw Expr -> NonEmpty (Raw Line) -> Raw Statement
if_ e sts =
  CompoundStatement $
  If () (Indents [] ()) [Space] e
    (SuiteMany () [] (LF Nothing) $ toBlock sts)
    []
    Nothing

ifElse_
  :: Raw Expr
  -> NonEmpty (Raw Line)
  -> NonEmpty (Raw Line)
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
  -> NonEmpty (Raw Line)
  -> NonEmpty (Raw Line)
  -> Raw Statement
forElse_ val vals block els =
  CompoundStatement $
  For () (Indents [] ()) Nothing [Space] (val & trailingWhitespace .~ [Space]) [Space] vals
    (SuiteMany () [] (LF Nothing) $ toBlock block)
    (Just (Indents [] (), [], SuiteMany () [] (LF Nothing) $ toBlock els))

for_ :: Raw Expr -> Raw Expr -> NonEmpty (Raw Line) -> Raw Statement
for_ val vals block =
  CompoundStatement $
  For () (Indents [] ()) Nothing [Space] (val & trailingWhitespace .~ [Space]) [Space] vals
    (SuiteMany () [] (LF Nothing) $ toBlock block)
    Nothing
