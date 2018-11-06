{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}

{-|
Module      : Language.Python.Internal.Parse
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Parse where

import Control.Applicative (Alternative, (<|>), optional, many, some)
import Control.Lens.Getter ((^.))
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List (foldl')
import Data.List.NonEmpty (some1)
import Data.Proxy (Proxy(..))
import Data.Void (Void)
import Text.Megaparsec
  ((<?>), MonadParsec, Parsec, ParseError, Stream(..), SourcePos(..), eof, try, lookAhead)
import Text.Megaparsec.Char (satisfy)


import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Megaparsec as Megaparsec

import Language.Python.Internal.Lexer (SrcInfo(..), withSrcInfo)
import Language.Python.Internal.Syntax.AugAssign
import Language.Python.Internal.Syntax.Comment
import Language.Python.Internal.Syntax.IR
import Language.Python.Internal.Syntax.Import
import Language.Python.Internal.Syntax.Numbers
import Language.Python.Internal.Syntax.Strings
import Language.Python.Internal.Token
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Ident
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Whitespace

newtype PyTokens = PyTokens { unPyTokens :: [PyToken SrcInfo] }
  deriving (Eq, Ord)

instance Stream PyTokens where
  type Token PyTokens = PyToken SrcInfo
  type Tokens PyTokens = PyTokens
  tokenToChunk Proxy = PyTokens . pure
  tokensToChunk Proxy = PyTokens
  chunkToTokens Proxy = unPyTokens
  chunkLength Proxy = length . unPyTokens
  chunkEmpty Proxy = null . unPyTokens
  positionAt1 Proxy _ tk =
    let
      ann = pyTokenAnn tk
    in
      SourcePos
        (_srcInfoName ann)
        (Megaparsec.mkPos $ _srcInfoLineStart ann)
        (Megaparsec.mkPos $ _srcInfoColStart ann)
  positionAtN Proxy spos (PyTokens tks) =
    case tks of
      [] -> spos
      _ ->
        let
          ann = pyTokenAnn $ last tks
        in
          SourcePos
            (_srcInfoName ann)
            (Megaparsec.mkPos $ _srcInfoLineStart ann)
            (Megaparsec.mkPos $ _srcInfoColStart ann)
  advance1 Proxy _ _ tk =
    let
      ann = pyTokenAnn tk
    in
      SourcePos
        (_srcInfoName ann)
        (Megaparsec.mkPos $ _srcInfoLineEnd ann)
        (Megaparsec.mkPos $ _srcInfoColEnd ann)
  advanceN Proxy _ spos (PyTokens tks) =
    case tks of
      [] -> spos
      _ ->
        let
          ann = pyTokenAnn $ last tks
        in
          SourcePos
            (_srcInfoName ann)
            (Megaparsec.mkPos $ _srcInfoLineEnd ann)
            (Megaparsec.mkPos $ _srcInfoColEnd ann)

  take1_ (PyTokens p) =
    case p of
      [] -> Nothing
      t:ts -> Just (t, PyTokens ts)

  takeN_ n (PyTokens s)
    | n <= 0    = Just (PyTokens [], PyTokens s)
    | null s    = Nothing
    | otherwise = Just (coerce (splitAt n s))

  takeWhile_ f = coerce (span f)

type Parser = Parsec Void PyTokens

{-# inline runParser #-}
runParser
  :: String
  -> Parser a
  -> [PyToken SrcInfo]
  -> Either (ParseError (PyToken SrcInfo) Void) a
runParser file p input = Megaparsec.parse p file (PyTokens input)

eol :: MonadParsec e PyTokens m => m Newline
eol =
  (\(TkNewline nl _) -> nl) <$>
  satisfy (\case; TkNewline{} -> True; _ -> False) <?> "newline"

dedent :: MonadParsec e PyTokens m => m ()
dedent = () <$ satisfy (\case; TkDedent{} -> True; _ -> False) <?> "dedent"

space :: MonadParsec e PyTokens m => m Whitespace
space =
  Space <$ satisfy (\case; TkSpace{} -> True; _ -> False) <|>
  Tab <$ satisfy (\case; TkTab{} -> True; _ -> False) <|>
  continued

continued :: MonadParsec e PyTokens m => m Whitespace
continued =
  (\(TkContinued nl _) -> Continued nl) <$>
  satisfy (\case; TkContinued{} -> True; _ -> False) <*>
  many space

newline :: MonadParsec e PyTokens m => m Newline
newline = (\(TkNewline nl _) -> nl) <$> satisfy (\case; TkNewline{} -> True; _ -> False)

anySpace :: MonadParsec e PyTokens m => m Whitespace
anySpace =
  Space <$ satisfy (\case; TkSpace{} -> True; _ -> False) <|>
  Tab <$ satisfy (\case; TkTab{} -> True; _ -> False) <|>
  continued <|>
  Newline <$> newline <|>
  Comment . void <$> comment

token
  :: MonadParsec e PyTokens m
  => m Whitespace
  -> (PyToken SrcInfo -> Bool)
  -> String
  -> m (PyToken SrcInfo, [Whitespace])
token ws f label = (,) <$> satisfy f <*> many ws <?> label

identifier :: MonadParsec e PyTokens m => m Whitespace -> m (Ident '[] SrcInfo)
identifier ws =
  (\(TkIdent n ann) -> MkIdent ann n) <$>
  satisfy (\case; TkIdent{} -> True; _ -> False) <*>
  many ws

bool :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
bool ws =
  (\(tk, s) ->
     Bool
       (pyTokenAnn tk)
       (case tk of
          TkTrue{} -> True
          TkFalse{} -> False
          _ -> error "impossible")
       s) <$>
  (token ws (\case; TkTrue{} -> True; _ -> False) "True" <|>
   token ws (\case; TkFalse{} -> True; _ -> False) "False")

none :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
none ws =
  (\(tk, s) -> None (pyTokenAnn tk) s) <$>
  token ws (\case; TkNone{} -> True; _ -> False) "None"

ellipsis :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
ellipsis ws =
  (\(tk, s) -> Ellipsis (pyTokenAnn tk) s) <$>
  token ws (\case; TkEllipsis{} -> True; _ -> False) "..."

integer :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
integer ws =
  (\(TkInt n) -> Int (_intLiteralAnn n) n) <$>
  satisfy (\case; TkInt{} -> True; _ -> False) <*>
  many ws

float :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
float ws =
  (\(TkFloat n) -> Float (_floatLiteralAnn n) n) <$>
  satisfy (\case; TkFloat{} -> True; _ -> False) <*>
  many ws

imag :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
imag ws =
  (\(TkImag n) -> Imag (_imagLiteralAnn n) n) <$>
  satisfy (\case; TkImag{} -> True; _ -> False) <*>
  many ws

stringOrBytes :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
stringOrBytes ws =
  fmap (\vs -> String (_stringLiteralAnn $ NonEmpty.head vs) vs) . some1 $
  (\case
     TkString sp qt st val ann -> StringLiteral ann sp qt st val
     TkBytes sp qt st val ann -> BytesLiteral ann sp qt st val
     TkRawString sp st qt val ann -> RawStringLiteral ann sp st qt val
     TkRawBytes sp st qt val ann -> RawBytesLiteral ann sp st qt val
     _ -> error "impossible") <$>
  satisfy
    (\case
        TkString{} -> True
        TkBytes{} -> True
        TkRawString{} -> True
        TkRawBytes{} -> True
        _ -> False) <*>
  many ws

comment :: MonadParsec e PyTokens m => m (Comment SrcInfo)
comment =
  (\(TkComment c) -> c) <$>
  satisfy (\case; TkComment{} -> True; _ -> False) <?> "comment"

indent :: MonadParsec e PyTokens m => m (Indents SrcInfo)
indent =
  (\(TkIndent _ i) -> i) <$> satisfy (\case; TkIndent{} -> True; _ -> False) <?> "indent"

level :: MonadParsec s PyTokens m => m (Indents SrcInfo)
level =
  (\(TkLevel _ i) -> i) <$> satisfy (\case; TkLevel{} -> True; _ -> False) <?> "level indentation"

exprList :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
exprList ws =
  (\e -> maybe e (uncurry $ Tuple (e ^. exprAnn) e)) <$>
  expr ws <*>
  optional
    ((,) <$>
     (snd <$> comma ws) <*>
     optional (commaSep1' ws $ expr ws))

exprOrStarList :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
exprOrStarList ws =
  (\e -> maybe e (uncurry $ Tuple (e ^. exprAnn) e)) <$>
  (expr ws <|> starExpr ws) <*>
  optional
    ((,) <$>
     (snd <$> comma ws) <*>
     optional (commaSep1' ws $ expr ws <|> starExpr ws))

compIf :: MonadParsec e PyTokens m => m (CompIf SrcInfo)
compIf =
  (\(tk, s) -> CompIf (pyTokenAnn tk) s) <$>
  token anySpace (\case; TkIf{} -> True; _ -> False) "if" <*>
  exprNoCond anySpace

compFor :: MonadParsec e PyTokens m => m (CompFor SrcInfo)
compFor =
  (\(tk, s) -> CompFor (pyTokenAnn tk) s) <$>
  token anySpace (\case; TkFor{} -> True; _ -> False) "for" <*>
  orExprList anySpace <*>
  (snd <$> token anySpace (\case; TkIn{} -> True; _ -> False) "in") <*>
  orTest anySpace

-- | (',' x)* [',']
commaSepRest :: MonadParsec e PyTokens m => m b -> m ([(Comma, b)], Maybe Comma)
commaSepRest x = do
  c <- optional $ snd <$> comma anySpace
  case c of
    Nothing -> pure ([], Nothing)
    Just c' -> do
      e <- optional x
      case e of
        Nothing -> pure ([], Just c')
        Just e' -> first ((c', e') :) <$> commaSepRest x

exprComp :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
exprComp ws =
  (\ex a ->
     case a of
       Nothing -> ex
       Just (cf, rest) ->
         Generator (ex ^. exprAnn) $
         Comprehension (ex ^. exprAnn) ex cf rest) <$>
  expr ws <*>
  optional ((,) <$> compFor <*> many (Left <$> compFor <|> Right <$> compIf))

starExpr :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
starExpr ws =
  (\(tk, sp) -> StarExpr (pyTokenAnn tk) sp) <$>
  token ws (\case; TkStar{} -> True; _ -> False) "*" <*>
  orExpr ws

exprListComp :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
exprListComp ws =
  (\e a ->
     case a of
       Left (cf, cfs) ->
         let
           ann = e ^. exprAnn
         in
           Generator ann $ Comprehension ann e cf cfs
       Right (Just (c, cs)) -> Tuple (e ^. exprAnn) e c cs
       Right Nothing -> e) <$>
  (expr ws <|> starExpr ws) <*>
  (Left <$>
   ((,) <$>
    compFor <*>
    many (Left <$> compFor <|> Right <$> compIf)) <|>
   Right <$>
   optional
     ((,) <$>
      (snd <$> comma ws) <*>
      optional (commaSep1' ws $ expr ws <|> starExpr ws)))

orExprList :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
orExprList ws =
  (\e -> maybe e (uncurry $ Tuple (e ^. exprAnn) e)) <$>
  (orExpr ws <|> starExpr ws) <*>
  optional
    ((,) <$>
     (snd <$> comma ws) <*>
     optional (commaSep1' ws $ orExpr ws <|> starExpr ws))

binOp :: MonadParsec e PyTokens m => m (BinOp SrcInfo) -> m (Expr SrcInfo) -> m (Expr SrcInfo)
binOp op tm =
  (\t ts ->
      case ts of
        [] -> t
        _ -> foldl (\tm (o, val) -> BinOp (tm ^. exprAnn) tm o val) t ts) <$>
  tm <*>
  many ((,) <$> op <*> tm)

orTest :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
orTest ws = binOp orOp andTest
  where
    orOp =
      (\(tk, ws) -> BoolOr (pyTokenAnn tk) ws) <$>
      token ws (\case; TkOr{} -> True; _ -> False) "or"

    andOp =
      (\(tk, ws) -> BoolAnd (pyTokenAnn tk) ws) <$>
      token ws (\case; TkAnd{} -> True; _ -> False) "and"
    andTest = binOp andOp notTest

    notTest =
      (\(tk, s) -> Not (pyTokenAnn tk) s) <$>
      token ws (\case; TkNot{} -> True; _ -> False) "not" <*> notTest <|>
      comparison

    compOp =
      (\(tk, ws) -> maybe (Is (pyTokenAnn tk) ws) (IsNot (pyTokenAnn tk) ws)) <$>
      token ws (\case; TkIs{} -> True; _ -> False) "is" <*>
      optional (snd <$> token ws (\case; TkNot{} -> True; _ -> False) "not")

      <|>

      (\(tk, ws) -> NotIn (pyTokenAnn tk) ws) <$>
      token ws (\case; TkNot{} -> True; _ -> False) "not" <*>
      (snd <$> token ws (\case; TkIn{} -> True; _ -> False) "in")

      <|>

      (\(tk, ws) -> In (pyTokenAnn tk) ws) <$>
      token ws (\case; TkIn{} -> True; _ -> False) "in"

      <|>

      (\(tk, ws) -> Equals (pyTokenAnn tk) ws) <$>
      token ws (\case; TkDoubleEq{} -> True; _ -> False) "=="

      <|>

      (\(tk, ws) -> Lt (pyTokenAnn tk) ws) <$>
      token ws (\case; TkLt{} -> True; _ -> False) "<"

      <|>

      (\(tk, ws) -> LtEquals (pyTokenAnn tk) ws) <$>
      token ws (\case; TkLte{} -> True; _ -> False) "<="

      <|>

      (\(tk, ws) -> Gt (pyTokenAnn tk) ws) <$>
      token ws (\case; TkGt{} -> True; _ -> False) ">"

      <|>

      (\(tk, ws) -> GtEquals (pyTokenAnn tk) ws) <$>
      token ws (\case; TkGte{} -> True; _ -> False) ">="

      <|>

      (\(tk, ws) -> NotEquals (pyTokenAnn tk) ws) <$>
      token ws (\case; TkBangEq{} -> True; _ -> False) "!="

    comparison = binOp compOp $ orExpr ws

yieldExpr :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
yieldExpr ws =
  (\(tk, s) -> either (uncurry $ YieldFrom (pyTokenAnn tk) s) (Yield (pyTokenAnn tk) s)) <$>
  token ws (\case; TkYield{} -> True; _ -> False) "yield" <*>
  (fmap Left
     ((,) <$>
      (snd <$> token ws (\case; TkFrom{} -> True; _ -> False) "from") <*>
      expr ws)
     <|>
   Right <$> optional (exprList ws))

lambda :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
lambda ws =
  (\(tk, s) -> Lambda (pyTokenAnn tk) s) <$>
  token ws (\case; TkLambda{} -> True; _ -> False) "lambda" <*>
  commaSep ws untypedParam <*>
  (snd <$> token ws (\case; TkColon{} -> True; _ -> False) ":") <*>
  expr ws

lambdaNoCond :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
lambdaNoCond ws =
  (\(tk, s) -> Lambda (pyTokenAnn tk) s) <$>
  token ws (\case; TkLambda{} -> True; _ -> False) "lambda" <*>
  commaSep ws untypedParam <*>
  (snd <$> token ws (\case; TkColon{} -> True; _ -> False) ":") <*>
  exprNoCond ws

exprNoCond :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
exprNoCond ws = orTest ws <|> lambdaNoCond ws

expr :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
expr ws =
  (\a -> maybe a (\(b, c, d, e) -> Ternary (a ^. exprAnn) a b c d e)) <$>
  orTest ws <*>
  optional
    ((,,,) <$>
     (snd <$> token ws (\case; TkIf{} -> True; _ -> False) "if") <*>
     orTest ws <*>
     (snd <$> token ws (\case; TkElse{} -> True; _ -> False) "else") <*>
     expr ws)
  <|>
  lambda ws

orExpr :: MonadParsec e PyTokens m => m Whitespace -> m (Expr SrcInfo)
orExpr ws =
  binOp
    ((\(tk, ws) -> BitOr (pyTokenAnn tk) ws) <$>
     token ws (\case; TkPipe{} -> True; _ -> False) "|")
    xorExpr
  where
    xorExpr =
      binOp
        ((\(tk, ws) -> BitXor (pyTokenAnn tk) ws) <$>
         token ws (\case; TkCaret{} -> True; _ -> False) "^")
        andExpr

    andExpr =
      binOp
        ((\(tk, ws) -> BitAnd (pyTokenAnn tk) ws) <$>
         token ws (\case; TkAmpersand{} -> True; _ -> False) "&")
        shiftExpr

    shiftExpr =
      binOp
        ((\(tk, ws) -> ShiftLeft (pyTokenAnn tk) ws) <$>
         token ws (\case; TkShiftLeft{} -> True; _ -> False) "<<"

         <|>

         (\(tk, ws) -> ShiftRight (pyTokenAnn tk) ws) <$>
         token ws (\case; TkShiftRight{} -> True; _ -> False) ">>")
        arithExpr

    arithOp =
      (\(tk, ws) -> Plus (pyTokenAnn tk) ws) <$>
      token ws (\case; TkPlus{} -> True; _ -> False) "+"

      <|>

      (\(tk, ws) -> Minus (pyTokenAnn tk) ws) <$>
      token ws (\case; TkMinus{} -> True; _ -> False) "-"

    arithExpr = binOp arithOp term

    termOp =
      (\(tk, ws) -> Multiply (pyTokenAnn tk) ws) <$>
      token ws (\case; TkStar{} -> True; _ -> False) "*"

      <|>

      (\(tk, ws) -> At (pyTokenAnn tk) ws) <$>
      token ws (\case; TkAt{} -> True; _ -> False) "@"

      <|>

      (\(tk, ws) -> Divide (pyTokenAnn tk) ws) <$>
      token ws (\case; TkSlash{} -> True; _ -> False) "/"

      <|>

      (\(tk, ws) -> FloorDivide (pyTokenAnn tk) ws) <$>
      token ws (\case; TkDoubleSlash{} -> True; _ -> False) "//"

      <|>

      (\(tk, ws) -> Percent (pyTokenAnn tk) ws) <$>
      token ws (\case; TkPercent{} -> True; _ -> False) "%"

    term = binOp termOp factor

    factor =
      ((\(tk, s) -> let ann = pyTokenAnn tk in UnOp ann (Negate ann s)) <$>
       token ws (\case; TkMinus{} -> True; _ -> False) "-"
       <|>
       (\(tk, s) -> let ann = pyTokenAnn tk in UnOp ann (Positive ann s)) <$>
       token ws (\case; TkPlus{} -> True; _ -> False) "+"
       <|>
       (\(tk, s) -> let ann = pyTokenAnn tk in UnOp ann (Complement ann s)) <$>
       token ws (\case; TkTilde{} -> True; _ -> False) "~") <*> factor
      <|>
      power

    powerOp =
      (\(tk, ws) -> Exp (pyTokenAnn tk) ws) <$>
      token ws (\case; TkDoubleStar{} -> True; _ -> False) "**"
    power =
      (\a -> maybe a (uncurry $ BinOp (a ^. exprAnn) a)) <$>
      atomExpr <*>
      optional ((,) <$> powerOp <*> factor)

    subscript = do
      mex <- optional $ expr anySpace
      case mex of
        Nothing ->
          SubscriptSlice Nothing <$>
          (snd <$> colon anySpace) <*>
          optional (expr anySpace) <*>
          optional ((,) <$> (snd <$> colon anySpace) <*> optional (expr anySpace))
        Just ex -> do
          mws <- optional $ snd <$> colon anySpace
          case mws of
            Nothing -> pure $ SubscriptExpr ex
            Just ws ->
              SubscriptSlice (Just ex) ws <$>
              optional (expr anySpace) <*>
              optional ((,) <$> (snd <$> colon anySpace) <*> optional (expr anySpace))

    trailer =
      (\a b c -> Deref (c ^. exprAnn) c a b) <$>
      (snd <$> token ws (\case; TkDot{} -> True; _ -> False) ".") <*>
      identifier ws

      <|>

      (\a b c d -> Call (d ^. exprAnn) d a b c) <$>
      (snd <$> token anySpace (\case; TkLeftParen{} -> True; _ -> False) "(") <*>
      optional (commaSep1' anySpace arg) <*>
      (snd <$> token ws (\case; TkRightParen{} -> True; _ -> False) ")")

      <|>

      (\a b c d -> Subscript (d ^. exprAnn) d a b c) <$>
      (snd <$> token anySpace (\case; TkLeftBracket{} -> True; _ -> False) "[") <*>
      commaSep1' anySpace subscript <*>
      (snd <$> token ws (\case; TkRightBracket{} -> True; _ -> False) "]")

    atomExpr =
      (\(mAwait, a) b ->
         let e = foldl' (&) a b
         in maybe e (\(tk, sp) -> Await (pyTokenAnn tk) sp e) mAwait) <$>
      try
        ((,) <$>
         optional (token ws (\case; TkIdent "await" _ -> True; _ -> False) "await") <*>
         atom) <*>
      many trailer
      <|>
      foldl' (&) <$> atom <*> many trailer

    parensOrUnit =
      (\(tk, s) maybeEx sps ->
       case maybeEx of
         Nothing -> Unit (pyTokenAnn tk) s sps
         Just ex -> Parens (pyTokenAnn tk) s ex sps) <$>
      token anySpace (\case; TkLeftParen{} -> True; _ -> False) "(" <*>
      optional (yieldExpr anySpace <|> exprListComp anySpace) <*>
      (snd <$> token ws (\case; TkRightParen{} -> True; _ -> False) ")")

    list =
      (\(tk, sp1) ->
         maybe (List (pyTokenAnn tk) sp1 Nothing) (\f -> f (pyTokenAnn tk) sp1)) <$>
      token anySpace (\case; TkLeftBracket{} -> True; _ -> False) "[" <*>
      optional
        ((\e a ann ws1 ->
          case a of
            Left (cf, cfs) -> ListComp ann ws1 (Comprehension (e ^. exprAnn) e cf cfs)
            Right Nothing -> List ann ws1 (Just $ CommaSepOne1' e Nothing)
            Right (Just (c, Nothing)) -> List ann ws1 (Just $ CommaSepOne1' e $ Just c)
            Right (Just (c, Just cs)) -> List ann ws1 (Just $ CommaSepMany1' e c cs)) <$>
        (expr anySpace <|> starExpr anySpace) <*>
        (Left <$>
        ((,) <$>
          compFor <*>
          many (Left <$> compFor <|> Right <$> compIf)) <|>
        Right <$>
        optional
          ((,) <$>
           (snd <$> comma anySpace) <*>
           optional (commaSep1' anySpace (expr anySpace <|> starExpr anySpace))))) <*>
      (snd <$> token ws (\case; TkRightBracket{} -> True; _ -> False) "]")

    doubleStarExpr ws =
      (\(tk, sp) -> DictUnpack (pyTokenAnn tk) sp) <$>
      token ws (\case; TkDoubleStar{} -> True; _ -> False) "**" <*>
      orExpr ws

    dictItem =
      (\a -> DictItem (a ^. exprAnn) a) <$>
      expr anySpace <*>
      (snd <$> colon anySpace) <*>
      expr anySpace
      <|>
      doubleStarExpr anySpace

    compRHS = (,) <$> compFor <*> many (Left <$> compFor <|> Right <$> compIf)

    dictOrSet = do
      (a, ws1) <- token anySpace (\case; TkLeftBrace{} -> True; _ -> False) "{"
      let ann = pyTokenAnn a
      maybeExpr <-
        optional $
          Left . Left <$> expr anySpace <|>
          Left . Right <$> starExpr anySpace <|>
          Right <$> doubleStarExpr anySpace
      (case maybeExpr of
         Nothing -> pure $ Dict ann ws1 Nothing
         Just (Left (Left ex)) -> do
           maybeColon <-
             optional $ snd <$> token anySpace (\case; TkColon{} -> True; _ -> False) ":"
           case maybeColon of
             Nothing ->
               -- The order of this choice matters because commaSepRest is implemented
               -- in a slightly odd way
               (\(c, d) -> SetComp ann ws1 (Comprehension (ex ^. exprAnn) ex c d)) <$>
               compRHS
               <|>

               (\(rest, final) -> Set ann ws1 ((ex, rest, final) ^. _CommaSep1')) <$>
               commaSepRest (expr anySpace <|> starExpr anySpace)
             Just clws ->
               (\ex2 a ->
                 let
                   dictItemAnn = ex ^. exprAnn
                   firstDictItem = DictItem dictItemAnn ex clws ex2
                 in
                 case a of
                   Left (c, d) ->
                     DictComp ann ws1 (Comprehension dictItemAnn firstDictItem c d)
                   Right (rest, final) ->
                     Dict ann ws1 (Just $ (firstDictItem, rest, final) ^. _CommaSep1')) <$>
               expr anySpace <*>
               (Left <$> compRHS <|> Right <$> commaSepRest dictItem)
         Just (Left (Right ex)) ->
           ((\(c, d) -> SetComp ann ws1 (Comprehension (ex ^. exprAnn) ex c d)) <$>
            compRHS

            <|>

            (\(rest, final) -> Set ann ws1 ((ex, rest, final) ^. _CommaSep1')) <$>
            commaSepRest (expr anySpace <|> starExpr anySpace))
         Just (Right ex) ->
           ((\(c, d) -> DictComp ann ws1 (Comprehension (_dictItemAnn ex) ex c d)) <$>
            compRHS

            <|>

            (\(rest, final) -> Dict ann ws1 (Just $ (ex, rest, final) ^. _CommaSep1')) <$>
            commaSepRest dictItem)) <*>

        (snd <$> token ws (\case; TkRightBrace{} -> True; _ -> False) "}")

    atom =
      dictOrSet <|>
      list <|>
      none ws <|>
      bool ws <|>
      ellipsis ws <|>
      integer ws <|>
      float ws <|>
      imag ws <|>
      stringOrBytes ws <|>
      Ident <$> identifier ws <|>
      parensOrUnit

smallStatement :: MonadParsec e PyTokens m => m (SmallStatement SrcInfo)
smallStatement =
  returnSt <|>
  passSt <|>
  breakSt <|>
  continueSt <|>
  globalSt <|>
  nonlocalSt <|>
  delSt <|>
  importSt <|>
  raiseSt <|>
  exprOrAssignSt <|>
  yieldSt <|>
  assertSt
  where
    assertSt =
      (\(tk, s) -> Assert (pyTokenAnn tk) s) <$>
      token space (\case; TkAssert{} -> True; _ -> False) "assert" <*>
      expr space <*>
      optional ((,) <$> (snd <$> comma space) <*> expr space)

    yieldSt = (\a -> Expr (a ^. exprAnn) a) <$> yieldExpr space

    returnSt =
      (\(tkReturn, retSpaces) -> Return (pyTokenAnn tkReturn) retSpaces) <$>
      token space (\case; TkReturn{} -> True; _ -> False) "return" <*>
      optional (exprList space)

    passSt =
      uncurry (Pass . pyTokenAnn) <$>
      token space (\case; TkPass{} -> True; _ -> False) "pass"

    breakSt =
      uncurry (Break . pyTokenAnn) <$>
      token space (\case; TkBreak{} -> True; _ -> False) "break"

    continueSt =
      uncurry (Continue . pyTokenAnn) <$>
      token space (\case; TkContinue{} -> True; _ -> False) "continue"

    augAssign =
      (\(tk, s) -> MkAugAssign PlusEq (pyTokenAnn tk) s) <$>
      token space (\case; TkPlusEq{} -> True; _ -> False) "+="

      <|>

      (\(tk, s) -> MkAugAssign MinusEq (pyTokenAnn tk) s) <$>
      token space (\case; TkMinusEq{} -> True; _ -> False) "-="

      <|>

      (\(tk, s) -> MkAugAssign AtEq (pyTokenAnn tk) s) <$>
      token space (\case; TkAtEq{} -> True; _ -> False) "@="

      <|>

      (\(tk, s) -> MkAugAssign StarEq (pyTokenAnn tk) s) <$>
      token space (\case; TkStarEq{} -> True; _ -> False) "*="

      <|>

      (\(tk, s) -> MkAugAssign SlashEq (pyTokenAnn tk) s) <$>
      token space (\case; TkSlashEq{} -> True; _ -> False) "/="

      <|>

      (\(tk, s) -> MkAugAssign PercentEq (pyTokenAnn tk) s) <$>
      token space (\case; TkPercentEq{} -> True; _ -> False) "%="

      <|>

      (\(tk, s) -> MkAugAssign AmpersandEq (pyTokenAnn tk) s) <$>
      token space (\case; TkAmpersandEq{} -> True; _ -> False) "&="

      <|>

      (\(tk, s) -> MkAugAssign PipeEq (pyTokenAnn tk) s) <$>
      token space (\case; TkPipeEq{} -> True; _ -> False) "|="

      <|>

      (\(tk, s) -> MkAugAssign CaretEq (pyTokenAnn tk) s) <$>
      token space (\case; TkCaretEq{} -> True; _ -> False) "^="

      <|>

      (\(tk, s) -> MkAugAssign ShiftLeftEq (pyTokenAnn tk) s) <$>
      token space (\case; TkShiftLeftEq{} -> True; _ -> False) "<<="

      <|>

      (\(tk, s) -> MkAugAssign ShiftRightEq (pyTokenAnn tk) s) <$>
      token space (\case; TkShiftRightEq{} -> True; _ -> False) ">>="

      <|>

      (\(tk, s) -> MkAugAssign DoubleStarEq (pyTokenAnn tk) s) <$>
      token space (\case; TkDoubleStarEq{} -> True; _ -> False) "**="

      <|>

      (\(tk, s) -> MkAugAssign DoubleSlashEq (pyTokenAnn tk) s) <$>
      token space (\case; TkDoubleSlashEq{} -> True; _ -> False) "//="

    exprOrAssignSt =
      (\a ->
         maybe
           (Expr (a ^. exprAnn) a)
           (either
              (Assign (a ^. exprAnn) a)
              (uncurry $ AugAssign (a ^. exprAnn) a))) <$>
      exprOrStarList space <*>
      optional
        (Left <$>
         some1
           ((,) <$>
            (snd <$> token space (\case; TkEq{} -> True; _ -> False) "=") <*>
            (yieldExpr space <|> exprOrStarList space))

           <|>

         Right <$> ((,) <$> augAssign <*> (yieldExpr space <|> exprList space)))

    globalSt =
      (\(tk, s) -> Global (pyTokenAnn tk) $ NonEmpty.fromList s) <$>
      token space (\case; TkGlobal{} -> True; _ -> False) "global" <*>
      commaSep1 space (identifier space)

    nonlocalSt =
      (\(tk, s) -> Nonlocal (pyTokenAnn tk) $ NonEmpty.fromList s) <$>
      token space (\case; TkNonlocal{} -> True; _ -> False) "nonlocal" <*>
      commaSep1 space (identifier space)

    delSt =
      (\(tk, s) -> Del (pyTokenAnn tk) s) <$>
      token space (\case; TkDel{} -> True; _ -> False) "del" <*>
      commaSep1' space (orExpr space)

    raiseSt =
      (\(tk, s) -> Raise (pyTokenAnn tk) s) <$>
      token space (\case; TkRaise{} -> True; _ -> False) "raise" <*>
      optional
        ((,) <$>
         expr space <*>
         optional
           ((,) <$>
            (snd <$> token space (\case; TkFrom{} -> True; _ -> False) "from") <*>
            expr space))

    importSt = importName <|> importFrom
      where
        moduleName =
          makeModuleName <$>
          identifier space <*>
          many
            ((,) <$>
             (snd <$> token space (\case; TkDot{} -> True; _ -> False) ".") <*>
             identifier space)

        importAs ws getAnn p =
          (\a -> ImportAs (getAnn a) a) <$>
          p <*>
          optional
            ((,) <$>
             (NonEmpty.fromList . snd <$> token ws (\case; TkAs{} -> True; _ -> False) "as") <*>
             identifier ws)

        importName =
          (\(tk, s) -> Import (pyTokenAnn tk) $ NonEmpty.fromList s) <$>
          token space (\case; TkImport{} -> True; _ -> False) "import" <*>
          commaSep1 space (importAs space _moduleNameAnn moduleName)

        dots =
          fmap concat . some $
          pure . Dot . snd <$> token space (\case; TkDot{} -> True; _ -> False) "."

          <|>

          (\(_, ws) -> [Dot [], Dot [], Dot ws]) <$>
          token space (\case; TkEllipsis{} -> True; _ -> False) "..."

        relativeModuleName =
          RelativeWithName [] <$> moduleName

          <|>

          (\a -> maybe (Relative $ NonEmpty.fromList a) (RelativeWithName a)) <$>
          dots <*>
          optional moduleName

        importTargets =
          (\(tk, s) -> ImportAll (pyTokenAnn tk) s) <$>
          token space (\case; TkStar{} -> True; _ -> False) "*"

          <|>

          (\(tk, s) -> ImportSomeParens (pyTokenAnn tk) s) <$>
          token anySpace (\case; TkLeftParen{} -> True; _ -> False) "(" <*>
          commaSep1' anySpace (importAs anySpace _identAnn (identifier anySpace)) <*>
          (snd <$> token space (\case; TkRightParen{} -> True; _ -> False) ")")

          <|>

          (\a -> ImportSome (importAsAnn $ commaSep1Head a) a) <$>
          commaSep1 space (importAs space _identAnn (identifier space))

        importFrom =
          (\(tk, s) -> From (pyTokenAnn tk) s) <$>
          token space (\case; TkFrom{} -> True; _ -> False) "from" <*>
          relativeModuleName <*>
          (snd <$> token space (\case; TkImport{} -> True; _ -> False) "import") <*>
          importTargets

sepBy1' :: MonadParsec e PyTokens m => m a -> m sep -> m (a, [(sep, a)], Maybe sep)
sepBy1' val sep = go
  where
    go =
      (\a b ->
         case b of
           Nothing -> (a, [], Nothing)
           Just (sc, b') ->
             case b' of
               Nothing -> (a, [], Just sc)
               Just (a', ls, sc') -> (a, (sc, a') : ls, sc')) <$>
      val <*>
      optional ((,) <$> sep <*> optional go)

simpleStatement
  :: MonadParsec e PyTokens m
  => m (SimpleStatement SrcInfo)
simpleStatement =
  (\(a, b, c) d -> MkSimpleStatement a b c d) <$>
  sepBy1' smallStatement (snd <$> semicolon space) <*>
  optional comment <*>
  optional eol

statement
  :: (Alternative m, MonadParsec e PyTokens m)
  => m (Indents SrcInfo)
  -> Indents SrcInfo
  -> m (Statement SrcInfo)
statement pIndent indentBefore =
  -- It's important to parse compound statements first, because the 'async' keyword
  -- is actually an identifier and we'll have to backtrack
  CompoundStatement <$> compoundStatement pIndent indentBefore <|>
  SimpleStatement indentBefore <$> simpleStatement

blank :: MonadParsec e PyTokens m => m (Blank SrcInfo)
blank =
  withSrcInfo $
  (\b c a -> Blank a b c) <$>
  some space <*>
  optional comment

  <|>

  (\b a -> Blank a [] b) <$> optional comment

suite :: MonadParsec e PyTokens m => m (Suite SrcInfo)
suite =
  (\(tk, s) ->
     either
       (SuiteOne (pyTokenAnn tk) s)
       (\(a, b,c ) -> SuiteMany (pyTokenAnn tk) s a b c)) <$>
  colon space <*>
  (Left <$> simpleStatement

    <|>

   (fmap Right $
    (,,) <$>
    optional comment <*>
    eol <*>
    (Block <$>
     many ((,) <$> blank <*> eol) <*>
     (statement level =<< indent) <*>
     many (line level)) <*
    dedent))
  where

    line i =
      Left <$> ((,) <$> blank <*> eol) <|>
      Right <$> (statement level =<< i)

comma :: MonadParsec e PyTokens m => m Whitespace -> m (PyToken SrcInfo, Comma)
comma ws = fmap Comma <$> token ws (\case; TkComma{} -> True; _ -> False) ","

colon :: MonadParsec e PyTokens m => m Whitespace -> m (PyToken SrcInfo, [Whitespace])
colon ws = token ws (\case; TkColon{} -> True; _ -> False) ":"

semicolon :: MonadParsec e PyTokens m => m Whitespace -> m (PyToken SrcInfo, [Whitespace])
semicolon ws = token ws (\case; TkSemicolon{} -> True; _ -> False) ";"

commaSep :: MonadParsec e PyTokens m => m Whitespace -> m a -> m (CommaSep a)
commaSep ws pa =
  (\a -> maybe (CommaSepOne a) (uncurry $ CommaSepMany a)) <$>
  pa <*>
  optional ((,) <$> (snd <$> comma ws) <*> commaSep ws pa)

  <|>

  pure CommaSepNone

commaSep1 :: MonadParsec e PyTokens m => m Whitespace -> m a -> m (CommaSep1 a)
commaSep1 ws val = go
  where
    go =
      (\a -> maybe (CommaSepOne1 a) (uncurry $ CommaSepMany1 a)) <$>
      val <*>
      optional ((,) <$> (snd <$> comma ws) <*> go)

commaSep1' :: MonadParsec e PyTokens m => m Whitespace -> m a -> m (CommaSep1' a)
commaSep1' ws pa =
  (\(a, b, c) -> from a b c) <$> sepBy1' pa (snd <$> comma ws)
  where
    from a [] b = CommaSepOne1' a b
    from a ((b, c) : bs) d = CommaSepMany1' a b $ from c bs d

untypedParam :: MonadParsec e PyTokens m => m (Param SrcInfo)
untypedParam =
  (\a b ->
     maybe
       (PositionalParam (_identAnn a) a b)
       (uncurry $ KeywordParam (_identAnn a) a b)) <$>
  identifier anySpace <*>
  pure Nothing <*>
  optional
    ((,) <$>
     (snd <$> token anySpace (\case; TkEq{} -> True; _ -> False) "=") <*>
     expr anySpace)

  <|>

  (\(a, b) -> StarParam (pyTokenAnn a) b) <$>
  token anySpace (\case; TkStar{} -> True; _ -> False) "*" <*>
  optional (identifier anySpace) <*>
  pure Nothing

  <|>

  (\(a, b) -> DoubleStarParam (pyTokenAnn a) b) <$>
  token anySpace (\case; TkDoubleStar{} -> True; _ -> False) "**" <*>
  identifier anySpace <*>
  pure Nothing

typedParam :: MonadParsec e PyTokens m => m (Param SrcInfo)
typedParam =
  (\a b ->
     maybe
       (PositionalParam (_identAnn a) a b)
       (uncurry $ KeywordParam (_identAnn a) a b)) <$>
  identifier anySpace <*>
  optional tyAnn <*>
  optional
    ((,) <$>
     (snd <$> token anySpace (\case; TkEq{} -> True; _ -> False) "=") <*>
     expr anySpace)

  <|>

  (\(a, b) ->
     maybe
       (StarParam (pyTokenAnn a) b Nothing Nothing)
       (\(c, d) -> StarParam (pyTokenAnn a) b (Just c) d)) <$>
  token anySpace (\case; TkStar{} -> True; _ -> False) "*" <*>
  optional ((,) <$> identifier anySpace <*> optional tyAnn)

  <|>

  (\(a, b) -> DoubleStarParam (pyTokenAnn a) b) <$>
  token anySpace (\case; TkDoubleStar{} -> True; _ -> False) "**" <*>
  identifier anySpace <*>
  optional tyAnn
  where
    tyAnn =
      (,) <$>
      (snd <$> token anySpace (\case; TkColon{} -> True; _ -> False) ":") <*>
      expr anySpace

arg :: MonadParsec e PyTokens m => m (Arg SrcInfo)
arg =
  (do
      e <- exprComp anySpace
      case e of
        Ident ident -> do
          eqSpaces <-
            optional $ snd <$> token anySpace (\case; TkEq{} -> True; _ -> False) "="
          case eqSpaces of
            Nothing -> pure $ PositionalArg (e ^. exprAnn) e
            Just s -> KeywordArg (e ^. exprAnn) ident s <$> expr anySpace
        _ -> pure $ PositionalArg (e ^. exprAnn) e)

  <|>

  (\a -> PositionalArg (a ^. exprAnn) a) <$> expr anySpace

  <|>

  (\(a, b) -> StarArg (pyTokenAnn a) b) <$>
  token anySpace (\case; TkStar{} -> True; _ -> False) "*" <*>
  expr anySpace

  <|>

  (\(a, b) -> DoubleStarArg (pyTokenAnn a) b) <$>
  token anySpace (\case; TkDoubleStar{} -> True; _ -> False) "**" <*>
  expr anySpace

decoratorValue :: MonadParsec e PyTokens m => m (Expr SrcInfo)
decoratorValue = do
  id1 <- identifier space
  ids <-
    many
      ((,) <$>
       (snd <$> token space (\case; TkDot{} -> True; _ -> False) ".") <*>
       identifier space)
  args <-
    optional $
    (,,) <$>
    (snd <$> token anySpace (\case; TkLeftParen{} -> True; _ -> False) "(") <*>
    optional (commaSep1' anySpace arg) <*>
    (snd <$> token space (\case; TkRightParen{} -> True; _ -> False) ")")
  let
    derefs =
      foldl
        (\b (ws, a) -> Deref (b ^. exprAnn) b ws a)
        (Ident id1)
        ids
  pure $
    case args of
      Nothing -> derefs
      Just (l, x, r) -> Call (derefs ^. exprAnn) derefs l x r

decorator
  :: MonadParsec e PyTokens m
  => Indents SrcInfo
  -> m (Decorator SrcInfo)
decorator indentBefore =
  (\(tk, spcs) a b -> Decorator (pyTokenAnn tk) indentBefore spcs a b) <$>
  token space (\case; TkAt{} -> True; _ -> False) "@" <*>
  decoratorValue <*>
  optional comment <*>
  eol <*>
  many ((,) <$> blank <*> eol)

decorators
  :: MonadParsec e PyTokens m
  => m (Indents SrcInfo)
  -> Indents SrcInfo
  -> m [Decorator SrcInfo]
decorators pIndent indentBefore =
  (:) <$>
  decorator indentBefore <*>
  many (try i >>= decorator)
  where
    i =
      pIndent <*
      lookAhead (token space (\case; TkAt{} -> True; _ -> False) "@")

compoundStatement
  :: MonadParsec e PyTokens m
  => m (Indents SrcInfo)
  -> Indents SrcInfo
  -> m (CompoundStatement SrcInfo)
compoundStatement pIndent indentBefore =
  ifSt <|>
  whileSt <|>
  trySt <|>
  decorated <|>
  asyncSt <|>
  classSt indentBefore [] <|>
  fundef indentBefore Nothing [] <|>
  withSt Nothing <|>
  forSt Nothing
  where
    decorated = do
      ds <- decorators pIndent indentBefore
      i <- pIndent
      (do; a <- doAsync; fundef i (Just a) ds) <|>
        fundef i Nothing ds <|>
        classSt i ds

    classSt ib decs =
      (\(tk, s) a b c ->
        ClassDef
          (pyTokenAnn tk)
          decs
          ib
          (NonEmpty.fromList s) a b c) <$>
      token space (\case; TkClass{} -> True; _ -> False) "class" <*>
      identifier space <*>
      optional
        ((,,) <$>
         (snd <$> token anySpace (\case; TkLeftParen{} -> True; _ -> False) "(") <*>
         optional (commaSep1' anySpace arg) <*>
         (snd <$> token space (\case; TkRightParen{} -> True; _ -> False) ")")) <*>
      suite

    ifSt =
      (\(tk, s) a b c d -> If (pyTokenAnn tk) indentBefore s a b c d) <$>
      token space (\case; TkIf{} -> True; _ -> False) "if" <*>
      expr space <*>
      suite <*>
      many
        (try
           ((,,,) <$>
            pIndent <*>
            (snd <$> token space (\case; TkElif{} -> True; _ -> False) "elif")) <*>
         expr space <*>
         suite) <*>
      optional
        (try
           ((,,) <$>
            pIndent <*>
            (snd <$> token space (\case; TkElse{} -> True; _ -> False) "else")) <*>
         suite)

    whileSt =
      (\(tk, s) a b -> While (pyTokenAnn tk) indentBefore s a b) <$>
      token space (\case; TkWhile{} -> True; _ -> False) "while" <*>
      expr space <*>
      suite <*>
      optional
        (try
           ((,,) <$>
            pIndent <*>
            (snd <$> token space (\case; TkElse{} -> True; _ -> False) "else")) <*>
         suite)

    exceptAs =
      (\a -> ExceptAs (a ^. exprAnn) a) <$>
      expr space <*>
      optional
        ((,) <$>
         (snd <$> token space (\case; TkAs{} -> True; _ -> False) "as") <*>
         identifier space)

    trySt =
      (\(tk, s) a d ->
         case d of
           Left (e, f, g) -> TryFinally (pyTokenAnn tk) indentBefore s a e f g
           Right (e, f, g) -> TryExcept (pyTokenAnn tk) indentBefore s a e f g) <$>
      token space (\case; TkTry{} -> True; _ -> False) "try" <*>
      suite <*>
      (fmap Left
         (try
            ((,,) <$>
             pIndent <*>
             (snd <$> token space (\case; TkFinally{} -> True; _ -> False) "finally")) <*>
          suite)

        <|>

        fmap Right
          ((,,) <$>
           some1
             (try
                ((,,,) <$>
                 pIndent <*>
                 (snd <$> token space (\case; TkExcept{} -> True; _ -> False) "except")) <*>
              optional exceptAs <*>
              suite) <*>
           optional
             (try
                ((,,) <$>
                 pIndent <*>
                 (snd <$> token space (\case; TkElse{} -> True; _ -> False) "else")) <*>
              suite) <*>
           optional
             (try
                ((,,) <$>
                 pIndent <*>
                 (snd <$> token space (\case; TkFinally{} -> True; _ -> False) "finally")) <*>
              suite)))

    doAsync = token space (\case; TkIdent "async" _ -> True; _ -> False) "async"

    asyncSt = do
      a <-
        try $
        doAsync <*
        lookAhead
          (token space (\case; TkDef{} -> True; _ -> False) "def" <|>
           token space (\case; TkWith{} -> True; _ -> False) "with" <|>
           token space (\case; TkFor{} -> True; _ -> False) "for")
      fundef indentBefore (Just a) [] <|>
        withSt (Just a) <|>
        forSt (Just a)

    fundef ib async decs =
      (\(tkDef, defSpaces) a b c d e f ->
         Fundef
         (maybe (pyTokenAnn tkDef) (pyTokenAnn . fst) async)
         decs
         ib
         (NonEmpty.fromList . snd <$> async)
         (NonEmpty.fromList defSpaces)
         a b c d e f) <$>
      token space (\case; TkDef{} -> True; _ -> False) "def" <*>
      identifier space <*>
      fmap snd (token anySpace (\case; TkLeftParen{} -> True; _ -> False) "(") <*>
      commaSep anySpace typedParam <*>
      fmap snd (token space (\case; TkRightParen{} -> True; _ -> False) ")") <*>
      optional
        ((,) <$>
         (snd <$> token space (\case; TkRightArrow{} -> True; _ -> False) "->") <*>
         expr space) <*>
      suite

    withSt async =
      (\(tk, s) a b ->
          With
            (maybe (pyTokenAnn tk) (pyTokenAnn . fst) async)
            indentBefore
            (NonEmpty.fromList . snd <$> async)
            s a b) <$>
      token space (\case; TkWith{} -> True; _ -> False) "with" <*>
      commaSep1
        space
        ((\a -> WithItem (a ^. exprAnn) a) <$>
         expr space <*>
         optional
           ((,) <$>
            (snd <$> token space (\case; TkAs{} -> True; _ -> False) "as") <*>
            orExpr space)) <*>
      suite

    forSt async =
      (\(tk, s) a b c d e ->
        For
          (maybe (pyTokenAnn tk) (pyTokenAnn . fst) async)
          indentBefore
          (NonEmpty.fromList . snd <$> async)
          s a b c d e) <$>
      token space (\case; TkFor{} -> True; _ -> False) "for" <*>
      orExprList space <*>
      (snd <$> token space (\case; TkIn{} -> True; _ -> False) "in") <*>
      commaSep1' space (expr space) <*>
      suite <*>
      optional
        (try
           ((,,) <$>
            pIndent <*>
            (snd <$> token space (\case; TkElse{} -> True; _ -> False) "else")) <*>
         suite)

module_ :: MonadParsec e PyTokens m => m (Module SrcInfo)
module_ =
  ModuleStatement <$> (statement tlIndent =<< tlIndent) <*> module_

  <|>

  (\bl rest ->
     case rest of
       Left (nl, md) -> ModuleBlank bl nl md
       Right{} -> ModuleBlankFinal bl) <$>
  blank <*>
  (Left <$> ((,) <$> newline <*> module_) <|> Right <$> eof)

  <|>

  ModuleEmpty <$ eof

  where
    tlIndent = level <|> withSrcInfo (pure $ Indents [])
