{-# language DataKinds #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# language OverloadedLists #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
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
  , decorated_
  , HasDecorators(..)
    -- * Statements
    -- ** Lines of code
  , blank_
  , AsLine(..)
  , Line(..)
    -- ** Block bodies
  , HasBody(..)
  , modifyBody
    -- ** Function definitions
  , def_
  , Fundef(..)
  , _Fundef
  , mkFundef
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
    -- *** 'Else' clauses
  , else_
  , HasElse(..)
    -- *** Break
  , break_
    -- *** For loops
  , for_
  , For(..)
  , _For
  , mkFor
    -- *** If statements
  , if_
  , elif_
  , If(..)
  , _If
  , mkIf
  , Elif(..)
  , _Elif
  , mkElif
  , Else(..)
  , _Else
  , mkElse
    -- **** Lenses
  , ifAnn
  , ifIndents
  , ifIf
  , ifCond
  , ifBody
  , ifElifs
  , ifElse
  , elifIndents
  , elifElif
  , elifCond
  , elifBody
  , elseIndents
  , elseElse
  , elseBody
    -- *** Pass
  , pass_
    -- *** Return
  , return_
    -- *** While loops
  , while_
  , While(..)
  , _While
  , mkWhile
    -- * Expressions
  , expr_
  , var_
    -- ** Function calls
  , call_
  , Call(..)
  , _Call
  , mkCall
    -- *** Lenses
  , callAnn
  , callFunction
  , callLeftParen
  , callArguments
  , callRightParen
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
import Control.Lens.Setter ((.~), (<>~), Setter', over)
import Control.Lens.Traversal (traverseOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax hiding (Fundef, While, Call)
import Language.Python.Syntax.Types

type Raw f = f '[] ()

-- | Create a blank 'Line'
blank_ :: Raw Line
blank_ = Line $ Left ([], LF Nothing)

-- | One or more lines of Python code
newtype Line v a = Line { unLine :: Either ([Whitespace], Newline) (Statement v a) }
  deriving (Eq, Show)

-- | Convert some data to a 'Line'
class AsLine s where
  line_ :: Raw s -> Raw Line

instance AsLine SmallStatement where
  line_ ss =
    Line . Right $ SmallStatements (Indents [] ()) ss [] Nothing (Right $ LF Nothing)

instance AsLine CompoundStatement where
  line_ = Line . Right . CompoundStatement

instance AsLine If where
  line_ = line_ . (_If #)

instance AsLine Statement where
  line_ = Line . Right

instance AsLine Expr where
  line_ e = line_ $ Expr (e ^. exprAnn) e

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
          line_ (SmallStatements (Indents [] ()) c [] Nothing (Right d)) :| []
        SuiteMany a b c d ->
          Line <$> unBlock d) $
    fromMaybe
      (error "malformed indentation in function body")
      (traverseOf _Indents (fmap doDedent . subtractStart (_fdIndents fun)) (_fdBody fun))

instance HasParameters Fundef where
  setParameters p = fdParameters .~ listToCommaSep p
  parameters = fdParameters

-- | Create a minimal valid function definition
mkFundef :: Raw Ident -> NonEmpty (Raw Line) -> Raw Fundef
mkFundef name body =
  MkFundef
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

-- | Create a minimal valid 'Call'
mkCall :: Raw Expr -> Raw Call
mkCall e =
  MkCall
  { _callAnn = ()
  , _callFunction = e
  , _callLeftParen = []
  , _callArguments = Nothing
  , _callRightParen = []
  }

call_ :: Raw Expr -> [Raw Arg] -> Raw Expr
call_ expr args =
  _Call #
  (mkCall expr)
  { _callArguments = 
    case args of
      [] -> Nothing
      a:as -> Just $ (a, zip (repeat [Space]) as, Nothing) ^. _CommaSep1'
  }

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

instance HasBody While where
  body = whileBody

  setBody ws new fun =
    fun
    { _whileBody =
      over
        (_Indents.indentsValue)
        ((_whileIndents fun ^. indentsValue <>) . doIndent ws)
        (SuiteMany () [] (LF Nothing) . Block $ unLine <$> new)
    }

  getBody fun =
    (\case
        SuiteOne a b c d ->
          line_ (SmallStatements (Indents [] ()) c [] Nothing (Right d)) :| []
        SuiteMany a b c d ->
          Line <$> unBlock d) $
    fromMaybe
      (error "malformed indentation in while body")
      (traverseOf
         _Indents
         (fmap doDedent . subtractStart (_whileIndents fun))
         (_whileBody fun))

-- | Create a minimal valid 'While'
mkWhile :: Raw Expr -> NonEmpty (Raw Line) -> Raw While
mkWhile cond body =
  MkWhile
  { _whileAnn = ()
  , _whileIndents = Indents [] ()
  , _whileWhile = [Space]
  , _whileCond = cond
  , _whileBody = SuiteMany () [] (LF Nothing) $ toBlock body
  }

while_ :: Raw Expr -> NonEmpty (Raw Line) -> Raw Statement
while_ e sts = _While # mkWhile e sts

-- | Create a minimal valid 'If'
mkIf :: Raw Expr -> NonEmpty (Raw Line) -> Raw If
mkIf cond body =
  MkIf
  { _ifAnn = ()
  , _ifIndents = Indents [] ()
  , _ifIf = [Space]
  , _ifCond = cond
  , _ifBody = SuiteMany () [] (LF Nothing) $ toBlock body
  , _ifElifs = []
  , _ifElse = Nothing
  }

instance HasBody Elif where
  body = elifBody

  setBody ws new fun =
    fun
    { _elifBody =
      over
        (_Indents.indentsValue)
        ((_elifIndents fun ^. indentsValue <>) . doIndent ws)
        (SuiteMany () [] (LF Nothing) . Block $ unLine <$> new)
    }

  getBody fun =
    (\case
        SuiteOne a b c d ->
          line_ (SmallStatements (Indents [] ()) c [] Nothing (Right d)) :| []
        SuiteMany a b c d ->
          Line <$> unBlock d) $
    fromMaybe
      (error "malformed indentation in elif body")
      (traverseOf
         _Indents
         (fmap doDedent . subtractStart (_elifIndents fun))
         (_elifBody fun))

instance HasBody Else where
  body = elseBody

  setBody ws new fun =
    fun
    { _elseBody =
      over
        (_Indents.indentsValue)
        ((_elseIndents fun ^. indentsValue <>) . doIndent ws)
        (SuiteMany () [] (LF Nothing) . Block $ unLine <$> new)
    }

  getBody fun =
    (\case
        SuiteOne a b c d ->
          line_ (SmallStatements (Indents [] ()) c [] Nothing (Right d)) :| []
        SuiteMany a b c d ->
          Line <$> unBlock d) $
    fromMaybe
      (error "malformed indentation in else body")
      (traverseOf
         _Indents
         (fmap doDedent . subtractStart (_elseIndents fun))
         (_elseBody fun))

instance HasBody If where
  body = ifBody

  setBody ws new fun =
    fun
    { _ifBody =
      over
        (_Indents.indentsValue)
        ((_ifIndents fun ^. indentsValue <>) . doIndent ws)
        (SuiteMany () [] (LF Nothing) . Block $ unLine <$> new)
    }

  getBody fun =
    (\case
        SuiteOne a b c d ->
          line_ (SmallStatements (Indents [] ()) c [] Nothing (Right d)) :| []
        SuiteMany a b c d ->
          Line <$> unBlock d) $
    fromMaybe
      (error "malformed indentation in if body")
      (traverseOf
         _Indents
         (fmap doDedent . subtractStart (_ifIndents fun))
         (_ifBody fun))

if_ :: Raw Expr -> NonEmpty (Raw Line) -> Raw If
if_ cond body = mkIf cond body

var_ :: String -> Raw Expr
var_ s = Ident $ MkIdent () s []

none_ :: Raw Expr
none_ = None () []

pass_ :: Raw Statement
pass_ = SmallStatements (Indents [] ()) (Pass () []) [] Nothing (Right (LF Nothing))

-- | Create a minimal valid 'Elif'
mkElif :: Raw Expr -> NonEmpty (Raw Line) -> Raw Elif
mkElif cond body =
  MkElif
  { _elifIndents = Indents [] ()
  , _elifElif = [Space]
  , _elifCond = cond
  , _elifBody = SuiteMany () [] (LF Nothing) $ toBlock body
  }

elif_ :: Raw Expr -> NonEmpty (Raw Line) -> Raw If -> Raw If
elif_ cond body code = code & ifElifs <>~ [mkElif cond body]

-- | Create a minimal valid 'Else'
mkElse :: NonEmpty (Raw Line) -> Raw Else
mkElse body =
  MkElse
  { _elseIndents = Indents [] ()
  , _elseElse = []
  , _elseBody = SuiteMany () [] (LF Nothing) $ toBlock body
  }

class HasElse s where
  getElse :: Raw s -> Maybe (Raw Else)
  setElse :: [Whitespace] -> Maybe (Raw Else) -> Raw s -> Raw s

else_ :: HasElse s => NonEmpty (Raw Line) -> Raw s -> Raw s
else_ body = setElse (replicate 4 Space) $ Just (mkElse body)

mkGetElse
  :: (Raw s -> Indents ())
  -> (Raw s -> Maybe (Raw Else))
  -> Raw s
  -> Maybe (Raw Else)
mkGetElse indentLevel elseField code =
  fromMaybe
    (error "malformed indentation in else block")
    (traverseOf
        (traverse._Indents)
        (subtractStart (indentLevel code))
        (elseField code))

mkSetElse
  :: (Raw s -> Indents ())
  -> Setter' (Raw s) (Maybe (Raw Else))
  -> [Whitespace]
  -> Maybe (Raw Else)
  -> Raw s
  -> Raw s
mkSetElse indentLevel elseField ws new code =
  code &
  elseField .~
    (fmap (elseIndents .~ indentLevel code) $
     over
       (traverse._Indents.indentsValue)
       (indentLevel code ^. indentsValue <>)
       new)

instance HasElse For where
  getElse = mkGetElse _forIndents _forElse
  setElse = mkSetElse _forIndents forElse

instance HasElse If where
  getElse = mkGetElse _ifIndents _ifElse
  setElse = mkSetElse _ifIndents ifElse

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

mkFor :: Raw Expr -> Raw Expr -> NonEmpty (Raw Line) -> Raw For
mkFor binder collection body =
  MkFor
  { _forAnn = ()
  , _forIndents = Indents [] ()
  , _forAsync = Nothing
  , _forFor = [Space]
  , _forBinder = binder
  , _forIn = [Space]
  , _forCollection = collection
  , _forBody = SuiteMany () [] (LF Nothing) $ toBlock body
  , _forElse = Nothing
  }

for_ :: Raw Expr -> Raw Expr -> NonEmpty (Raw Line) -> Raw Statement
for_ val vals block = _For # mkFor val vals block
