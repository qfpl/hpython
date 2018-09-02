{-|

Passing @[]@ to a function which expects a @['Raw' 'Line']@ is the same as
passing @['line_' 'pass_']@

-}

{-# language DataKinds #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# language OverloadedLists #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
module Language.Python.Syntax
  ( (&)
  , Raw
  , Statement
  , Expr
    -- * Identifiers
  , id_
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
    -- ** @as@
  , As(..)
    -- ** @async@
  , HasAsync(..)
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
  , chainEq
  , (.=)
  , (.+=)
  , (.-=)
  , (.*=)
  , (.@=)
  , (./=)
  , (.%=)
  , (.&=)
  , (.|=)
  , (.^=)
  , (.<<=)
  , (.>>=)
  , (.**=)
  , (.//=)
  , (.>)
  , (.>=)
  , (.<)
  , (.<=)
  , (.!=)
    -- ** Exceptions
  , tryE_
  , tryF_
  , HasExcept(..)
  , HasFinally(..)
  , TryExcept(..)
  , _TryExcept
  , mkTryExcept
  , TryFinally(..)
  , _TryFinally
  , mkTryFinally
  , AsExceptAs(..)
  , Except(..)
  , _Except
  , mkExcept
  , Finally(..)
  , _Finally
  , mkFinally
    -- *** Lenses
  , teAnn
  , teIndents
  , teTry
  , teBody
  , teExcepts
  , teElse
  , teFinally
  , exceptIndents
  , exceptExcept
  , exceptExceptAs
  , exceptBody
  , finallyIndents
  , finallyFinally
  , finallyBody
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
  , isNot_
  , in_
  , notIn_
  , (.*)
  , (.-)
  , (.+)
  , (.==)
  , (.|)
  , (.^)
  , (.&)
  , (.<<)
  , (.>>)
  , (.@)
  , (./)
  , (.//)
  , (.%)
  , (.**)
  )
where

import Data.Function ((&))
import Data.String (fromString)
import Control.Lens.Getter ((^.))
import Control.Lens.Iso (from)
import Control.Lens.Lens (Lens')
import Control.Lens.Prism (_Right)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((.~), (<>~), (?~), (%~), Setter', over)
import Control.Lens.Traversal (traverseOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax hiding (Fundef, While, Call)
import Language.Python.Syntax.Types

type Raw f = f '[] ()

toNonEmptyLines :: [Raw Line] -> NonEmpty (Raw Line)
toNonEmptyLines [] = pure (line_ pass_)
toNonEmptyLines (a:as) = a :| as

-- | Create a blank 'Line'
blank_ :: Raw Line
blank_ = Line $ Left ([], LF Nothing)

-- | 'Ident' has an 'Data.String.IsString' instance, but when a type class dispatches on
-- an 'Ident' we will run into ambiguity if we try to use @OverloadedStrings@. In these
-- cases we can use 'id_' to provide the extra type information
id_ :: String -> Raw Ident
id_ = fromString

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
    -> [Raw Line] -- ^ Lines to become the new body
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
  getBody :: Raw s -> [Raw Line]
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
  -> ([Raw Line] -> [Raw Line]) -- ^ Modification function
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

mkSetBody
  :: HasIndents s
  => Setter' (Raw s) (Raw Suite)
  -> (Raw s -> Indents ())
  -> [Whitespace]
  -> [Raw Line]
  -> Raw s
  -> Raw s
mkSetBody bodyField indentsField ws new code =
  code &
  bodyField .~
    over
      (_Indents.indentsValue)
      ((indentsField code ^. indentsValue <>) . doIndent ws)
      (SuiteMany () [] (LF Nothing) . Block $ unLine <$> toNonEmptyLines new)

mkGetBody
  :: HasIndents s
  => String
  -> (Raw s -> Raw Suite)
  -> (Raw s -> Indents ())
  -> Raw s
  -> [Raw Line]
mkGetBody thing bodyField indentsField code =
  (\case
      SuiteOne a b c d ->
        [ line_ $ SmallStatements (Indents [] ()) c [] Nothing (Right d) ]
      SuiteMany a b c d ->
        NonEmpty.toList $ Line <$> unBlock d) $
  fromMaybe
    (error $ "malformed indentation in " <> thing <> " body")
    (traverseOf _Indents (fmap doDedent . subtractStart (indentsField code)) (bodyField code))

instance HasBody Fundef where
  body = fdBody
  setBody = mkSetBody fdBody _fdIndents
  getBody = mkGetBody "function" _fdBody _fdIndents

instance HasParameters Fundef where
  setParameters p = fdParameters .~ listToCommaSep p
  parameters = fdParameters

-- | Create a minimal valid function definition
mkFundef :: Raw Ident -> [Raw Line] -> Raw Fundef
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

def_ :: Raw Ident -> [Raw Param] -> [Raw Line] -> Raw Statement
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

mkBinOp :: ([Whitespace] -> BinOp ()) -> Raw Expr -> Raw Expr -> Raw Expr
mkBinOp bop a = BinOp () (a & trailingWhitespace .~ [Space]) (bop [Space])

is_ :: Raw Expr -> Raw Expr -> Raw Expr
is_ = mkBinOp $ Is ()
infixl 1 `is_`

in_ :: Raw Expr -> Raw Expr -> Raw Expr
in_ = mkBinOp $ In ()
infixl 1 `in_`

notIn_ :: Raw Expr -> Raw Expr -> Raw Expr
notIn_ = mkBinOp $ NotIn () [Space]
infixl 1 `notIn_`

isNot_ :: Raw Expr -> Raw Expr -> Raw Expr
isNot_ = mkBinOp $ IsNot () [Space]
infixl 1 `isNot_`

(.==) :: Raw Expr -> Raw Expr -> Raw Expr
(.==) = mkBinOp $ Equals ()
infixl 1 .==

(.<) :: Raw Expr -> Raw Expr -> Raw Expr
(.<) = mkBinOp $ Lt ()
infixl 1 .<

(.<=) :: Raw Expr -> Raw Expr -> Raw Expr
(.<=) = mkBinOp $ LtEquals ()
infixl 1 .<=

(.>) :: Raw Expr -> Raw Expr -> Raw Expr
(.>) = mkBinOp $ Gt ()
infixl 1 .>

(.>=) :: Raw Expr -> Raw Expr -> Raw Expr
(.>=) = mkBinOp $ GtEquals ()
infixl 1 .>=

(.!=) :: Raw Expr -> Raw Expr -> Raw Expr
(.!=) = mkBinOp $ NotEquals ()
infixl 1 .!=

(.|) :: Raw Expr -> Raw Expr -> Raw Expr
(.|) = mkBinOp $ BitOr ()
infixl 2 .|

(.^) :: Raw Expr -> Raw Expr -> Raw Expr
(.^) = mkBinOp $ BitXor ()
infixl 3 .^

(.&) :: Raw Expr -> Raw Expr -> Raw Expr 
(.&) = mkBinOp $ BitAnd ()
infixl 4 .&

(.<<) :: Raw Expr -> Raw Expr -> Raw Expr 
(.<<) = mkBinOp $ ShiftLeft ()
infixl 5 .<<

(.>>) :: Raw Expr -> Raw Expr -> Raw Expr 
(.>>) = mkBinOp $ ShiftRight ()
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
(.@) = mkBinOp $ At ()
infixl 7 .@

(./) :: Raw Expr -> Raw Expr -> Raw Expr
(./) = mkBinOp $ Divide ()
infixl 7 ./

(.//) :: Raw Expr -> Raw Expr -> Raw Expr
(.//) = mkBinOp $ FloorDivide ()
infixl 7 .//

(.%) :: Raw Expr -> Raw Expr -> Raw Expr
(.%) = mkBinOp $ Percent ()
infixl 7 .%

(.**) :: Raw Expr -> Raw Expr -> Raw Expr
(.**) = mkBinOp $ Exp ()
infixr 8 .**

(/>) :: Raw Expr -> Raw Ident -> Raw Expr
(/>) a b = Deref () a [] b
infixl 9 />

neg :: Raw Expr -> Raw Expr
neg = negate

toBlock :: [Raw Line] -> Block '[] ()
toBlock =
  over
    (_Indents.indentsValue)
    (doIndent $ replicate 4 Space) .
    Block . fmap unLine . toNonEmptyLines

instance HasBody While where
  body = whileBody
  setBody = mkSetBody whileBody _whileIndents
  getBody = mkGetBody "while" _whileBody _whileIndents

-- | Create a minimal valid 'While'
mkWhile :: Raw Expr -> [Raw Line] -> Raw While
mkWhile cond body =
  MkWhile
  { _whileAnn = ()
  , _whileIndents = Indents [] ()
  , _whileWhile = [Space]
  , _whileCond = cond
  , _whileBody = SuiteMany () [] (LF Nothing) $ toBlock body
  }

while_ :: Raw Expr -> [Raw Line] -> Raw Statement
while_ e sts = _While # mkWhile e sts

-- | Create a minimal valid 'If'
mkIf :: Raw Expr -> [Raw Line] -> Raw If
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
  setBody = mkSetBody elifBody _elifIndents
  getBody = mkGetBody "elif" _elifBody _elifIndents

instance HasBody Else where
  body = elseBody
  setBody = mkSetBody elseBody _elseIndents
  getBody = mkGetBody "else" _elseBody _elseIndents

instance HasBody If where
  body = ifBody
  setBody = mkSetBody ifBody _ifIndents
  getBody = mkGetBody "if" _ifBody _ifIndents

if_ :: Raw Expr -> [Raw Line] -> Raw If
if_ cond body = mkIf cond body

var_ :: String -> Raw Expr
var_ s = Ident $ MkIdent () s []

none_ :: Raw Expr
none_ = None () []

pass_ :: Raw Statement
pass_ = SmallStatements (Indents [] ()) (Pass () []) [] Nothing (Right (LF Nothing))

-- | Create a minimal valid 'Elif'
mkElif :: Raw Expr -> [Raw Line] -> Raw Elif
mkElif cond body =
  MkElif
  { _elifIndents = Indents [] ()
  , _elifElif = [Space]
  , _elifCond = cond
  , _elifBody = SuiteMany () [] (LF Nothing) $ toBlock body
  }

elif_ :: Raw Expr -> [Raw Line] -> Raw If -> Raw If
elif_ cond body code = code & ifElifs <>~ [mkElif cond body]

-- | Create a minimal valid 'Else'
mkElse :: [Raw Line] -> Raw Else
mkElse body =
  MkElse
  { _elseIndents = Indents [] ()
  , _elseElse = []
  , _elseBody = SuiteMany () [] (LF Nothing) $ toBlock body
  }

class HasElse s where
  getElse :: Raw s -> Maybe (Raw Else)
  setElse :: [Whitespace] -> Maybe (Raw Else) -> Raw s -> Raw s

else_ :: HasElse s => [Raw Line] -> Raw s -> Raw s
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

mkAugAssign :: ([Whitespace] -> AugAssign ()) -> Raw Expr -> Raw Expr -> Raw Statement
mkAugAssign as a b =
  SmallStatements
    (Indents [] ())
    (AugAssign () (a & trailingWhitespace .~ [Space]) (as [Space]) b)
    []
    Nothing
    (Right (LF Nothing))

-- | Chained assignment
--
-- >>> chainEq (var_ "a") []
-- a
--
-- >>> chainEq (var_ "a") [var_ "b", var_ "c"]
-- a = b = c
chainEq :: Raw Expr -> [Raw Expr] -> Raw Statement
chainEq t [] = expr_ t
chainEq t (a:as) =
  SmallStatements
    (Indents [] ())
    (Assign () t $ (,) [Space] <$> (a :| as))
    []
    Nothing
    (Right $ LF Nothing)

(.=) :: Raw Expr -> Raw Expr -> Raw Statement
(.=) a b =
  SmallStatements
    (Indents [] ())
    (Assign () (a & trailingWhitespace .~ [Space]) $ pure ([Space], b))
    []
    Nothing
    (Right $ LF Nothing)
infix 0 .=

(.+=) :: Raw Expr -> Raw Expr -> Raw Statement
(.+=) = mkAugAssign (PlusEq ())
infix 0 .+=

(.-=) :: Raw Expr -> Raw Expr -> Raw Statement
(.-=) = mkAugAssign (MinusEq ())
infix 0 .-=

(.*=) :: Raw Expr -> Raw Expr -> Raw Statement
(.*=) = mkAugAssign (StarEq ())
infix 0 .*=

(.@=) :: Raw Expr -> Raw Expr -> Raw Statement
(.@=) = mkAugAssign (AtEq ())
infix 0 .@=

(./=) :: Raw Expr -> Raw Expr -> Raw Statement
(./=) = mkAugAssign (SlashEq ())
infix 0 ./=

(.%=) :: Raw Expr -> Raw Expr -> Raw Statement
(.%=) = mkAugAssign (PercentEq ())
infix 0 .%=

(.&=) :: Raw Expr -> Raw Expr -> Raw Statement
(.&=) = mkAugAssign (AmpersandEq ())
infix 0 .&=

(.|=) :: Raw Expr -> Raw Expr -> Raw Statement
(.|=) = mkAugAssign (PipeEq ())
infix 0 .|=

(.^=) :: Raw Expr -> Raw Expr -> Raw Statement
(.^=) = mkAugAssign (CaretEq ())
infix 0 .^=

(.<<=) :: Raw Expr -> Raw Expr -> Raw Statement
(.<<=) = mkAugAssign (ShiftLeftEq ())
infix 0 .<<=

(.>>=) :: Raw Expr -> Raw Expr -> Raw Statement
(.>>=) = mkAugAssign (ShiftRightEq ())
infix 0 .>>=

(.**=) :: Raw Expr -> Raw Expr -> Raw Statement
(.**=) = mkAugAssign (DoubleStarEq ())
infix 0 .**=

(.//=) :: Raw Expr -> Raw Expr -> Raw Statement
(.//=) = mkAugAssign (DoubleSlashEq ())
infix 0 .//=

mkFor :: Raw Expr -> Raw Expr -> [Raw Line] -> Raw For
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

for_ :: Raw Expr -> Raw Expr -> [Raw Line] -> Raw Statement
for_ val vals block = _For # mkFor val vals block

instance HasBody For where
  body = forBody
  setBody = mkSetBody forBody _forIndents
  getBody = mkGetBody "for" _forBody _forIndents

instance AsLine For where
  line_ = line_ . (_For #)

class HasAsync s where
  async_ :: Raw s -> Raw s

instance HasAsync Fundef where
  async_ = fdAsync ?~ pure Space

instance HasAsync For where
  async_ = forAsync ?~ pure Space

-- | Create a minimal valid 'Finally'
mkFinally :: [Raw Line] -> Raw Finally
mkFinally body =
  MkFinally
  { _finallyIndents = Indents [] ()
  , _finallyFinally = []
  , _finallyBody = SuiteMany () [] (LF Nothing) $ toBlock body
  }

-- | Create a minimal valid 'Except'
mkExcept :: [Raw Line] -> Raw Except
mkExcept body =
  MkExcept
  { _exceptIndents = Indents [] ()
  , _exceptExcept = []
  , _exceptExceptAs = Nothing
  , _exceptBody = SuiteMany () [] (LF Nothing) $ toBlock body
  }

-- | Create a minimal valid 'TryExcept'
mkTryExcept :: [Raw Line] -> Raw Except -> Raw TryExcept
mkTryExcept body except =
  MkTryExcept
  { _teAnn = ()
  , _teIndents = Indents [] ()
  , _teTry = [Space]
  , _teBody = SuiteMany () [] (LF Nothing) $ toBlock body
  , _teExcepts = pure except
  , _teElse = Nothing
  , _teFinally = Nothing
  }

-- | Create a minimal valid 'TryFinally'
mkTryFinally :: [Raw Line] -> [Raw Line] -> Raw TryFinally
mkTryFinally body fBody =
  MkTryFinally
  { _tfAnn = ()
  , _tfIndents = Indents [] ()
  , _tfTry = [Space]
  , _tfBody = SuiteMany () [] (LF Nothing) $ toBlock body
  , _tfFinally = mkFinally fBody
  }

class HasFinally s where
  finally_ :: [Raw Line] -> Raw s -> Raw s

instance HasFinally TryExcept where
  finally_ body = teFinally ?~ mkFinally body

instance HasFinally TryFinally where
  finally_ body = tfFinally .~ mkFinally body

instance HasBody TryExcept where
  body = teBody
  setBody = mkSetBody teBody _teIndents
  getBody = mkGetBody "try except" _teBody _teIndents

tryE_ :: [Raw Line] -> Raw Except -> Raw TryExcept
tryE_ = mkTryExcept

instance HasBody TryFinally where
  body = tfBody
  setBody = mkSetBody tfBody _tfIndents
  getBody = mkGetBody "try finally" _tfBody _tfIndents

tryF_ :: [Raw Line] -> [Raw Line] -> Raw TryFinally
tryF_ = mkTryFinally

class AsExceptAs s where
  toExceptAs :: Raw s -> Raw ExceptAs

instance AsExceptAs ExceptAs where
  toExceptAs = id

instance AsExceptAs Expr where
  toExceptAs e = ExceptAs () e Nothing

class HasExcept s where
  except_ :: [Raw Line] -> s -> Raw TryExcept
  -- | You can use 'exceptAs_' without a binder:
  --
  -- @'exceptAs_' :: 'Raw' 'Expr' -> ['Raw' 'Line'] -> 'Raw' s -> 'Raw' 'TryExcept'@
  --
  -- @
  -- 'exceptAs_' ('var_' \"Exception\") body
  -- @
  --
  -- or with a binder:
  --
  -- @'exceptAs_' :: 'Raw' 'ExceptAs' -> ['Raw' 'Line'] -> 'Raw' s -> 'Raw' 'TryExcept'@
  --
  -- @
  -- 'exceptAs_' ('var_' \"Exception\" \``as_`\` 'id_' "a") body
  -- @
  exceptAs_ :: AsExceptAs e => Raw e -> [Raw Line] -> s -> Raw TryExcept

-- |
-- @
-- 'tryE_' ['line_' 'pass_'] '&'
--   'except_' ['line_' 'pass_']
-- @
--
-- @
-- 'tryE_' ['line_' 'pass_'] '&'
--   'exceptAs_' ('var_' \"Exception\" \``as_`\` 'id_' "b") ['line_' 'pass_']
-- @
instance HasExcept (Raw Except -> Raw TryExcept) where
  except_ body f = f $ mkExcept body
  exceptAs_ ea body f = f $ mkExcept body & exceptExceptAs ?~ toExceptAs ea

-- |
-- @
-- (someTryStatement :: 'Raw' 'TryExcept') '&'
--   'except_' ['line_' 'pass_']
-- @
--
-- @
-- (someTryStatement :: 'Raw' 'TryExcept') '&'
--   'exceptAs_' ('var_' \"Exception\" \``as_`\` 'id_' "b") ['line_' 'pass_']
-- @
instance HasExcept (Raw TryExcept) where
  except_ body = teExcepts %~ (<> pure (mkExcept body))
  exceptAs_ ea body =
    teExcepts %~ (<> pure (mkExcept body & exceptExceptAs ?~ toExceptAs ea))

-- |
-- @
-- (someTryStatement :: 'Raw' 'TryFinally') '&'
--   'except_' ['line_' 'pass_']
-- @
--
-- @
-- (someTryStatement :: 'Raw' 'TryFinally') '&'
--   'exceptAs_' ('var_' \"Exception\" \``as_`\` 'id_' "b") ['line_' 'pass_']
-- @
instance HasExcept (Raw TryFinally) where
  except_ body MkTryFinally{..} =
    MkTryExcept
    { _teAnn = _tfAnn
    , _teIndents = _tfIndents
    , _teTry = _tfTry
    , _teBody = _tfBody
    , _teExcepts = pure $ mkExcept body
    , _teElse = Nothing
    , _teFinally = Just _tfFinally
    }

  exceptAs_ ea body MkTryFinally{..} =
    MkTryExcept
    { _teAnn = _tfAnn
    , _teIndents = _tfIndents
    , _teTry = _tfTry
    , _teBody = _tfBody
    , _teExcepts = pure $ mkExcept body & exceptExceptAs ?~ toExceptAs ea
    , _teElse = Nothing
    , _teFinally = Just _tfFinally
    }

instance AsLine TryExcept where
  line_ = line_ . (_TryExcept #)

instance AsLine TryFinally where
  line_ = line_ . (_TryFinally #)

class As s t u | s t -> u, u -> s t where
  as_ :: Raw s -> Raw t -> Raw u

instance As Expr Ident ExceptAs where
  as_ e name = ExceptAs () e $ Just ([Space], name)
