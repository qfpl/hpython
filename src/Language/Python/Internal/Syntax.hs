{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language DataKinds, PolyKinds #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances,
  MultiParamTypeClasses #-}
module Language.Python.Internal.Syntax where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Lens
import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Plated
import Control.Lens.Prism
import Control.Lens.Traversal
import Control.Lens.Wrapped
import Data.Coerce
import Data.Functor
import Data.List.NonEmpty hiding (fromList)
import Data.Monoid
import Data.String
import GHC.Exts
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Highlight

reservedWords :: [String]
reservedWords =
  [ "False"
  , "class"
  , "finally"
  , "is"
  , "return"
  , "None"
  , "continue"
  , "for"
  , "lambda"
  , "try"
  , "True"
  , "def"
  , "from"
  , "nonlocal"
  , "while"
  , "and"
  , "del"
  , "global"
  , "not"
  , "with"
  , "as"
  , "elif"
  , "if"
  , "or"
  , "yield"
  , "assert"
  , "else"
  , "import"
  , "pass"
  , "break"
  , "except"
  , "in"
  , "raise"
  ]

idStyle :: CharParsing m => IdentifierStyle (Unspaced m)
idStyle =
  IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = letter <|> char '_'
  , _styleLetter = letter <|> char '_' <|> digit
  , _styleReserved = fromList reservedWords
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved s = runUnspaced $ reserve idStyle s

data Ident (v :: [*]) a
  = MkIdent
  { _identAnnotation :: a
  , _identValue :: String
  } deriving (Eq, Show, Functor)
instance IsString (Ident '[] ()) where
  fromString = MkIdent ()
identValue :: Lens (Ident v a) (Ident '[] a) String String
identValue = lens _identValue (\s a -> s { _identValue = a })

identAnnotation :: Lens (Ident v a) (Ident v a) a a
identAnnotation = lens _identAnnotation (\s a -> s { _identAnnotation = a })

data Param (v :: [*]) a
  = PositionalParam
  { _paramAnn :: a
  , _paramName :: Ident v a
  }
  | KeywordParam
  { _paramAnn :: a
  , _paramName :: Ident v a
  , _unsafeKeywordParamWhitespaceLeft :: [Whitespace]
  , _unsafeKeywordParamWhitespaceRight :: [Whitespace]
  , _unsafeKeywordParamExpr :: Expr v a
  }
  deriving (Eq, Show)
instance HasExprs Param where
  _Exprs f (KeywordParam a name ws1 ws2 expr) =
    KeywordParam a (coerce name) <$> pure ws1 <*> pure ws2 <*> f expr
  _Exprs _ p@PositionalParam{} = pure $ coerce p

data Arg (v :: [*]) a
  = PositionalArg
  { _argAnn :: a
  , _argExpr :: Expr v a
  }
  | KeywordArg
  { _argAnn :: a
  , _unsafeKeywordArgName :: Ident v a
  , _unsafeKeywordArgWhitespaceLeft :: [Whitespace]
  , _unsafeKeywordArgWhitespaceRight :: [Whitespace]
  , _argExpr :: Expr v a
  }
  deriving (Eq, Show)
instance IsString (Arg '[] ()) where fromString = PositionalArg () . fromString
argExpr :: Lens (Arg v a) (Arg '[] a) (Expr v a) (Expr '[] a)
argExpr = lens _argExpr (\s a -> (coerce s) { _argExpr = a })
instance HasExprs Arg where
  _Exprs f (KeywordArg a name ws1 ws2 expr) = KeywordArg a (coerce name) ws1 ws2 <$> f expr
  _Exprs f (PositionalArg a expr) = PositionalArg a <$> f expr

data Whitespace = Space | Tab | Continued Newline [Whitespace] deriving (Eq, Show)

newtype Block v a = Block { unBlock :: NonEmpty (a, [Whitespace], Statement v a, Maybe Newline) }
  deriving (Eq, Show)
class HasBlocks s where
  _Blocks :: Traversal (s v a) (s '[] a) (Block v a) (Block '[] a)
instance HasBlocks Statement where
  _Blocks f (Fundef a ws1 name ws2 params ws3 ws4 nl b) =
    Fundef a ws1 (coerce name) ws2 (coerce params) ws3 ws4 nl <$> coerce (f b)
  _Blocks f (If a ws1 e1 ws2 ws3 nl b b') =
    If a ws1 (coerce e1) ws2 ws3 nl <$>
    coerce (f b) <*>
    traverseOf (traverse._4) (coerce . f) b'
  _Blocks f (While a ws1 e1 ws2 ws3 nl b) =
    While a ws1 (coerce e1) ws2 ws3 nl <$> coerce (f b)
  _Blocks _ s@Assign{} = pure $ coerce s
  _Blocks _ s@Expr{} = pure $ coerce s
  _Blocks _ s@Return{} = pure $ coerce s
  _Blocks _ s@Pass{} = pure $ coerce s
  _Blocks _ s@Break{} = pure $ coerce s

data Newline = CR | LF | CRLF deriving (Eq, Show)

data Statement (v :: [*]) a
  = Fundef a
      (NonEmpty Whitespace) (Ident v a)
      [Whitespace] (CommaSep (Param v a))
      [Whitespace] [Whitespace] Newline
      (Block v a)
  | Return a [Whitespace] (Expr v a)
  | Expr a (Expr v a)
  | If a
      [Whitespace] (Expr v a)
      [Whitespace] [Whitespace] Newline
      (Block v a)
      (Maybe ([Whitespace], [Whitespace], Newline, Block v a))
  | While a
      [Whitespace] (Expr v a)
      [Whitespace] [Whitespace] Newline
      (Block v a)
  | Assign a (Expr v a) [Whitespace] [Whitespace] (Expr v a)
  | Pass a
  | Break a
  deriving (Eq, Show)
instance Plated (Statement v a) where
  plate f (Fundef a ws1 b ws2 c ws3 ws4 nl sts) =
    Fundef a ws1 b ws2 c ws3 ws4 nl <$> (_Wrapped.traverse._3) f sts
  plate f (If a ws1 b ws2 ws3 nl sts sts') =
    If a ws1 b ws2 ws3 nl <$>
    (_Wrapped.traverse._3) f sts <*>
    (traverse._4._Wrapped.traverse._3) f sts'
  plate _ s@Return{} = pure $ coerce s
  plate _ s@Expr{} = pure $ coerce s
  plate _ s@Assign{} = pure $ coerce s
  plate f (While a ws1 b ws2 ws3 nl sts) =
    While a ws1 b ws2 ws3 nl <$> (_Wrapped.traverse._3) f sts
  plate _ s@Break{} = pure $ coerce s
  plate _ s@Pass{} = pure $ coerce s

data CommaSep a
  = CommaSepNone
  | CommaSepOne a
  | CommaSepMany a [Whitespace] [Whitespace] (CommaSep a)
  deriving (Eq, Show, Functor, Foldable, Traversable)
listToCommaSep :: [a] -> CommaSep a
listToCommaSep [] = CommaSepNone
listToCommaSep [a] = CommaSepOne a
listToCommaSep (a:as) = CommaSepMany a [] [Space] $ listToCommaSep as

data Expr (v :: [*]) a
  = List
  { _exprAnnotation :: a
  , _unsafeListWhitespaceLeft :: [Whitespace]
  , _unsafeListValues :: CommaSep (Expr v a)
  , _unsafeListWhitespaceRight :: [Whitespace]
  }
  | Deref
  { _exprAnnotation :: a
  , _unsafeDerefValueLeft :: Expr v a
  , _unsafeDerefWhitespaceLeft :: [Whitespace]
  , _unsafeDerefWhitespaceRight :: [Whitespace]
  , _unsafeDerefValueRight :: Ident v a
  }
  | Call
  { _exprAnnotation :: a
  , _unsafeCallFunction :: Expr v a
  , _unsafeCallWhitespace :: [Whitespace]
  , _unsafeCallArguments :: CommaSep (Arg v a)
  }
  | None
  { _exprAnnotation :: a
  }
  | BinOp
  { _exprAnnotation :: a
  , _unsafeBinOpExprLeft :: Expr v a
  , _unsafeBinOpWhitespaceLeft :: [Whitespace]
  , _unsafeBinOpOp :: BinOp a
  , _unsafeBinOpWhitesepaceRight :: [Whitespace]
  , _unsafeBinOpExprRight :: Expr v a
  }
  | Negate
  { _exprAnnotation :: a
  , _unsafeNegateWhitespace :: [Whitespace]
  , _unsafeNegateValue :: Expr v a
  }
  | Parens
  { _exprAnnotation :: a
  , _unsafeParensWhitespaceLeft :: [Whitespace]
  , _unsafeParensValue :: Expr v a
  , _unsafeParensWhitespaceRight :: [Whitespace]
  }
  | Ident
  { _exprAnnotation :: a
  , _unsafeIdentValue :: Ident v a
  }
  | Int
  { _exprAnnotation :: a
  , _unsafeIntValue :: Integer
  }
  | Bool
  { _exprAnnotation :: a
  , _unsafeBoolValue :: Bool
  }
  | String
  { _exprAnnotation :: a
  , _unsafeStringValue :: String
  }
  deriving (Eq, Show)
instance IsString (Expr '[] ()) where
  fromString = Ident () . MkIdent ()
instance Num (Expr '[] ()) where
  fromInteger = Int ()
  negate = Negate () []
  (+) a = BinOp () a [Space] (Plus ()) [Space]
  (*) a = BinOp () a [Space] (Multiply ()) [Space]
  (-) a = BinOp () a [Space] (Minus ()) [Space]
  signum = undefined
  abs = undefined
instance Plated (Expr '[] ()) where
  plate f (Parens a ws1 e ws2) = Parens a ws1 <$> f e <*> pure ws2
  plate f (List a ws1 exprs ws2) = List a ws1 <$> traverse f exprs <*> pure ws2
  plate f (Deref a expr ws1 ws2 name) =
    Deref a <$> f expr <*> pure ws1 <*> pure ws2 <*> pure name
  plate f (Call a expr ws args) = Call a <$> f expr <*> pure ws <*> (traverse._Exprs) f args
  plate f (BinOp a e1 ws1 op ws2 e2) =
    (\e1' e2' -> BinOp a e1' ws1 op ws2 e2') <$> f e1 <*> f e2
  plate f (Negate a ws expr) = Negate a ws <$> f expr
  plate _ e@String{} = pure $ coerce e
  plate _ e@None{} = pure $ coerce e
  plate _ e@Bool{} = pure $ coerce e
  plate _ e@Ident{} = pure $ coerce e
  plate _ e@Int{} = pure $ coerce e

data BinOp a
  = Is a
  | Minus a
  | Exp a
  | BoolAnd a
  | BoolOr a
  | Multiply a
  | Divide a
  | Plus a
  | Equals a
  deriving (Eq, Show, Functor)

-- | 'Traversal' over all the expressions in a term
class HasExprs s where
  _Exprs :: Traversal (s v a) (s '[] a) (Expr v a) (Expr '[] a)

instance HasExprs Statement where
  _Exprs f (Fundef a ws1 name ws2 params ws3 ws4 nl sts) =
    Fundef a ws1 (coerce name) ws2 <$>
    (traverse._Exprs) f params <*>
    pure ws3 <*>
    pure ws4 <*>
    pure nl <*>
    (_Wrapped.traverse._3._Exprs) f sts
  _Exprs f (Return a ws e) = Return a ws <$> f e
  _Exprs f (Expr a e) = Expr a <$> f e
  _Exprs f (If a ws1 e ws2 ws3 nl sts sts') =
    If a ws1 <$>
    f e <*>
    pure ws2 <*>
    pure ws3 <*>
    pure nl <*>
    (_Wrapped.traverse._3._Exprs) f sts <*>
    (traverse._4._Wrapped.traverse._3._Exprs) f sts'
  _Exprs f (While a ws1 e ws2 ws3 nl sts) =
    While a ws1 <$>
    f e <*>
    pure ws2 <*>
    pure ws3 <*>
    pure nl <*>
    (_Wrapped.traverse._3._Exprs) f sts
  _Exprs f (Assign a e1 ws1 ws2 e2) = Assign a <$> f e1 <*> pure ws1 <*> pure ws2 <*> f e2
  _Exprs _ p@Pass{} = pure $ coerce p
  _Exprs _ p@Break{} = pure $ coerce p

-- | 'Traversal' over all the statements in a term
class HasStatements s where
  _Statements :: Traversal (s v a) (s '[] a) (Statement v a) (Statement '[] a)

instance HasStatements Block where
  _Statements = _Wrapped.traverse._3

data Assoc = L | R deriving (Eq, Show)
data OpEntry
  = OpEntry
  { _opOperator :: BinOp ()
  , _opPrec :: Int
  , _opAssoc :: Assoc
  }
makeLenses ''OpEntry

operatorTable :: [OpEntry]
operatorTable =
  [ entry BoolOr 4 L
  , entry BoolAnd 5 L
  , entry Is 10 L
  , entry Equals 10 L
  , entry Minus 20 L
  , entry Plus 20 L
  , entry Multiply 25 L
  , entry Divide 25 L
  , entry Exp 30 R
  ]
  where
    entry a = OpEntry (a ())

lookupOpEntry :: BinOp a -> [OpEntry] -> OpEntry
lookupOpEntry op =
  go (op $> ())
  where
    go op [] = error $ show op <> " not found in operator table"
    go op (x:xs)
      | x ^. opOperator == op = x
      | otherwise = go op xs

shouldBracketLeft :: BinOp a -> Expr v a -> Bool
shouldBracketLeft op left =
  let
    entry = lookupOpEntry op operatorTable

    lEntry =
      case left of
        BinOp _ _ _ lOp _ _ -> Just $ lookupOpEntry lOp operatorTable
        _ -> Nothing

    leftf =
      case entry ^. opAssoc of
        R | Just R <- lEntry ^? _Just.opAssoc -> True
        _ -> False

    leftf' =
      case (left, op) of
        (Negate{}, Exp{}) -> True
        _ -> maybe False (\p -> p < entry ^. opPrec) (lEntry ^? _Just.opPrec)
  in
    leftf || leftf'

shouldBracketRight :: BinOp a -> Expr v a -> Bool
shouldBracketRight op right =
  let
    entry = lookupOpEntry op operatorTable

    rEntry =
      case right of
        BinOp _ _ _ rOp _ _ -> Just $ lookupOpEntry rOp operatorTable
        _ -> Nothing

    rightf =
      case entry ^. opAssoc of
        L | Just L <- rEntry ^? _Just.opAssoc -> True
        _ -> False

    rightf' = maybe False (\p -> p < entry ^. opPrec) (rEntry ^? _Just.opPrec)
  in
    rightf || rightf'

makeWrapped ''Block
makeLenses ''Expr
