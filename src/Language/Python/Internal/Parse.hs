{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language MultiParamTypeClasses #-}
module Language.Python.Internal.Parse where

import Control.Lens.Fold (foldOf, folded)
import Control.Lens.Getter ((^.), use)
import Control.Lens.Setter ((.=), assign)
import Control.Lens.TH (makeLenses)
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.State (MonadState, get, put)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor.Apply (Apply(..))
import Data.Functor.Alt (Alt((<!>)), many, some, optional)
import Data.Functor.Classes (liftEq)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Sequence (viewl, ViewL(..))

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Lexer
import Language.Python.Internal.Syntax.AugAssign
import Language.Python.Internal.Syntax.BinOp
import Language.Python.Internal.Syntax.Comment
import Language.Python.Internal.Syntax.CommaSep
import Language.Python.Internal.Syntax.IR
import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.Import
import Language.Python.Internal.Syntax.ModuleNames
import Language.Python.Internal.Syntax.Numbers
import Language.Python.Internal.Syntax.Strings
import Language.Python.Internal.Syntax.UnOp
import Language.Python.Internal.Syntax.Whitespace
import Language.Python.Internal.Token

some1 :: (Alt f, Applicative f) => f a -> f (NonEmpty a)
some1 a = (:|) <$> a <*> many a

data ParseError ann
  = UnexpectedEndOfInput ann
  | UnexpectedEndOfLine ann
  | UnexpectedEndOfBlock ann
  | UnexpectedIndent ann
  | ExpectedIndent ann
  | ExpectedEndOfBlock { peGotCtxt :: Line ann }
  | ExpectedIdentifier { peGot :: PyToken ann }
  | ExpectedContinued { peGot :: PyToken ann }
  | ExpectedNewline { peGot :: PyToken ann }
  | ExpectedStringOrBytes { peGot :: PyToken ann }
  | ExpectedInteger { peGot :: PyToken ann }
  | ExpectedFloat { peGot :: PyToken ann }
  | ExpectedImag { peGot :: PyToken ann }
  | ExpectedComment { peGot :: PyToken ann }
  | ExpectedToken { peExpected :: PyToken (), peGot :: PyToken ann }
  | ExpectedEndOfLine { peGotTokens :: [PyToken ann] }
  | ExpectedEndOfInput { peGotCtxt :: Line ann }
  deriving (Eq, Show)

data ParseState ann
  = ParserState
  { _parseLocation :: ann
  , _parseContext :: [[Either (Nested ann) (Line ann)]]
  }
makeLenses ''ParseState

firstLine :: Either (Nested a) (Line a) -> Line a
firstLine (Right a) = a
firstLine (Left a) =
  case viewl (unNested a) of
    EmptyL -> error "no line to return in firstLine"
    a :< _ -> firstLine a

newtype Parser ann a
  = Parser
  { unParser
    :: forall r
     . ParseState ann
    -> (ParseError ann -> r) -- Backtracking failure
    -> (ParseError ann -> r) -- Non-backtracking failure
    -> (a -> ParseState ann -> r) -- Backtracking success
    -> (a -> ParseState ann -> r) -- Non-backtracking success
    -> r
  }

instance Functor (Parser ann) where
  {-# inline fmap #-}
  fmap f (Parser g) =
    Parser $ \st btf err bts succ ->
    g st btf err (bts . f) (succ . f)

instance Apply (Parser ann) where
  {-# inline (<.>) #-}
  Parser mf <.> Parser ma =
    Parser $ \st btf err bts succ ->
    mf st
      btf
      err
      (\f st' -> ma st' btf err (bts . f) (succ . f))
      (\f st' -> let succ' = succ . f in ma st' err err succ' succ')

instance Applicative (Parser ann) where
  {-# inline pure #-}
  pure a = Parser $ \st _ _ bts _ -> bts a st
  {-# inline (<*>) #-}
  (<*>) = (<.>)

instance Monad (Parser ann) where
  {-# inline (>>=) #-}
  Parser ma >>= f =
    Parser $ \st btf err bts succ ->
    ma st
      btf
      err
      (\a st' -> unParser (f a) st' btf err bts succ)
      (\a st' -> unParser (f a) st' err err succ succ)

instance Alt (Parser ann) where
  {-# inline (<!>) #-}
  Parser ma <!> Parser mb =
    Parser $ \st btf err bts succ ->
    ma st (\_ -> mb st btf err bts succ) err bts succ

{-# inline try #-}
try :: Parser ann a -> Parser ann a
try (Parser ma) =
  Parser $ \st btf _ bts succ ->
  ma st btf btf bts succ

instance MonadState (ParseState ann) (Parser ann) where
  {-# inline get #-}
  get = Parser $ \st _ _ bts _ -> bts st st
  {-# inline put #-}
  put st = Parser $ \_ _ _ bts _ -> bts () st

instance MonadError (ParseError ann) (Parser ann) where
  {-# inline throwError #-}
  throwError e = Parser $ \_ btf _ _ _ -> btf e
  {-# inline catchError #-}
  catchError (Parser ma) f =
    Parser $ \st btf err bts succ ->
    ma st
      (\e -> unParser (f e) st btf err bts succ)
      (\e -> unParser (f e) st btf err bts succ)
      bts
      succ

{-# inline runParser #-}
runParser :: ann -> Parser ann a -> Nested ann -> Either (ParseError ann) a
runParser initial (Parser p) input =
  p
    (ParserState initial (pure . toList $ unNested input))
    Left
    Left
    (\a _ -> Right a)
    (\a _ -> Right a)

consumed :: Parser ann ()
consumed = Parser $ \st _ _ _ succ -> succ () st

currentToken :: Parser ann (PyToken ann)
currentToken = do
  ctxt <- use parseContext
  ann <- use parseLocation
  case ctxt of
    [] -> throwError $ UnexpectedEndOfInput ann
    current : rest ->
      case current of
        [] -> throwError $ UnexpectedEndOfBlock ann
        Right ll@(Line _ _ tks nl) : rest' ->
          case tks of
            [] -> throwError $ UnexpectedEndOfLine ann
            [tk] | Nothing <- nl -> do
              case rest' of
                [] -> assign parseContext rest
                _ -> assign parseContext $ rest' : rest
              pure tk
            tk : rest'' ->
              tk <$ assign parseContext ((Right (ll { lineLine = rest'' }) : rest') : rest)
        Left _ : _ -> throwError $ UnexpectedIndent ann

eol :: Parser ann Newline
eol = do
  ctxt <- use parseContext
  ann <- use parseLocation
  case ctxt of
    [] -> throwError $ UnexpectedEndOfInput ann
    current : rest ->
      case current of
        [] -> throwError $ UnexpectedEndOfBlock ann
        Left _ : _ -> throwError $ UnexpectedIndent ann
        Right (Line _ _ tks nl) : rest' ->
          case tks of
            _ : _ -> throwError $ ExpectedEndOfLine tks
            [] ->
              case nl of
                Nothing -> throwError $ ExpectedEndOfLine tks
                Just nl' -> do
                  consumed
                  nl' <$ (parseContext .= (rest' : rest))

eof :: Parser ann ()
eof = do
  ctxt <- use parseContext
  case ctxt of
    [] -> consumed
    x : xs ->
      case x of
        [] -> assign parseContext xs *> eof
        ls : _ -> throwError . ExpectedEndOfInput $ firstLine ls

indent :: Parser ann ()
indent = do
  ctxt <- use parseContext
  ann <- use parseLocation
  case ctxt of
    [] -> throwError $ UnexpectedEndOfInput ann
    current : rest ->
      case current of
        [] -> throwError $ UnexpectedEndOfBlock ann
        Right _ : _ -> throwError $ ExpectedIndent ann
        Left inner : rest' -> do
          consumed
          parseContext .= (toList (unNested inner) : rest' : rest)

dedent :: Parser ann ()
dedent = do
  ctxt <- use parseContext
  case ctxt of
    [] -> pure ()
    current : rest ->
      case current of
        [] -> do
          consumed
          parseContext .= rest
        ls : _ -> throwError . ExpectedEndOfBlock $ firstLine ls

consumed' :: ann -> Parser ann ()
consumed' ann = do
  consumed
  parseLocation .= ann

tokenEq :: PyToken b -> Parser ann (PyToken ann)
tokenEq tk = do
  curTk <- currentToken
  unless (liftEq (\_ _ -> True) tk curTk) . throwError $ ExpectedToken (() <$ tk) curTk
  curTk <$ consumed' (pyTokenAnn curTk)

space :: Parser ann Whitespace
space =
  Space <$ tokenEq (TkSpace ()) <!>
  Tab <$ tokenEq (TkTab ()) <!>
  continued

continued :: Parser ann Whitespace
continued = do
  curTk <- currentToken
  case curTk of
    TkContinued nl ann -> do
      consumed' ann
      Continued nl <$> many space
    _ -> throwError $ ExpectedContinued curTk

newline :: Parser ann Newline
newline = do
  curTk <- currentToken
  case curTk of
    TkNewline nl ann -> nl <$ consumed' ann
    _ -> throwError $ ExpectedNewline curTk

anySpace :: Parser ann Whitespace
anySpace =
  Space <$ tokenEq (TkSpace ()) <!>
  Tab <$ tokenEq (TkTab ()) <!>
  continued <!>
  Newline <$> newline

token :: Parser ann Whitespace -> PyToken b -> Parser ann (PyToken ann, [Whitespace])
token ws tk = do
  curTk <- tokenEq tk
  (,) curTk <$> many ws

identifier :: Parser ann Whitespace -> Parser ann (Ident '[] ann)
identifier ws = do
  curTk <- currentToken
  case curTk of
    TkIdent n ann -> do
      consumed' ann
      MkIdent ann n <$> many ws
    _ -> throwError $ ExpectedIdentifier curTk

bool :: Parser ann Whitespace -> Parser ann (Expr ann)
bool ws =
  (\(tk, s) ->
     Bool
       (pyTokenAnn tk)
       (case tk of
          TkTrue{} -> True
          TkFalse{} -> False
          _ -> error "impossible")
       s) <$>
  (token ws (TkTrue ()) <!> token ws (TkFalse ()))

none :: Parser ann Whitespace -> Parser ann (Expr ann)
none ws = (\(tk, s) -> None (pyTokenAnn tk) s) <$> token ws (TkNone ())

ellipsis :: Parser ann Whitespace -> Parser ann (Expr ann)
ellipsis ws = (\(tk, s) -> Ellipsis (pyTokenAnn tk) s) <$> token ws (TkEllipsis ())

integer :: Parser ann Whitespace -> Parser ann (Expr ann)
integer ws = do
  curTk <- currentToken
  case curTk of
    TkInt n -> do
      let ann = _intLiteralAnn n
      consumed' ann
      Int ann n <$> many ws
    _ -> throwError $ ExpectedInteger curTk

float :: Parser ann Whitespace -> Parser ann (Expr ann)
float ws = do
  curTk <- currentToken
  case curTk of
    TkFloat n -> do
      let ann = _floatLiteralAnn n
      consumed' ann
      Float ann n <$> many ws
    _ -> throwError $ ExpectedFloat curTk

imag :: Parser ann Whitespace -> Parser ann (Expr ann)
imag ws = do
  curTk <- currentToken
  case curTk of
    TkImag n -> do
      let ann = _imagLiteralAnn n
      consumed' ann
      Imag ann n <$> many ws
    _ -> throwError $ ExpectedImag curTk

stringOrBytes :: Parser ann Whitespace -> Parser ann (Expr ann)
stringOrBytes ws =
  fmap (\vs -> String (_stringLiteralAnn $ NonEmpty.head vs) vs) . some1 $ do
    curTk <- currentToken
    (case curTk of
       TkString sp qt st val ann ->
         StringLiteral ann sp qt st val <$ consumed' ann
       TkBytes sp qt st val ann ->
         BytesLiteral ann sp qt st val <$ consumed' ann
       TkRawString sp qt st val ann ->
         RawStringLiteral ann sp qt st val <$ consumed' ann
       TkRawBytes sp qt st val ann ->
         RawBytesLiteral ann sp qt st val <$ consumed' ann
       _ -> throwError $ ExpectedStringOrBytes curTk) <*>
     many ws

comment :: Parser ann Comment
comment = do
  curTk <- currentToken
  case curTk of
    TkComment str ann -> Comment str <$ consumed' ann
    _ -> throwError $ ExpectedComment curTk

between :: Parser ann left -> Parser ann right -> Parser ann a -> Parser ann a
between left right pa = left *> pa <* right

indents :: Parser ann (Indents ann)
indents = do
  ctxt <- use parseContext
  ann <- use parseLocation
  case ctxt of
    [] -> throwError $ UnexpectedEndOfInput ann
    current : _ ->
      case current of
        [] -> throwError $ UnexpectedEndOfBlock ann
        Right (Line a is _ _) : _ -> pure $ Indents is a
        Left _ : _ -> throwError $ UnexpectedIndent ann

exprList :: Parser ann Whitespace -> Parser ann (Expr ann)
exprList ws =
  (\e ->
     maybe
       e
       (uncurry $ Tuple (_exprAnn e) e)) <$>
  expr ws <*>
  optional
    ((,) <$>
     (snd <$> comma ws) <*>
     optional (commaSep1' ws $ expr ws))

compIf :: Parser ann (CompIf ann)
compIf =
  (\(tk, s) -> CompIf (pyTokenAnn tk) s) <$>
  token anySpace (TkIf ()) <*>
  exprNoCond anySpace

compFor :: Parser ann (CompFor ann)
compFor =
  (\(tk, s) -> CompFor (pyTokenAnn tk) s) <$>
  token anySpace (TkFor ()) <*>
  orExprList anySpace <*>
  (snd <$> token anySpace (TkIn ())) <*>
  orTest anySpace

-- | (',' x)* [',']
commaSepRest :: Parser ann b -> Parser ann ([([Whitespace], b)], Maybe [Whitespace])
commaSepRest x = do
  c <- optional $ snd <$> comma anySpace
  case c of
    Nothing -> pure ([], Nothing)
    Just c' -> do
      e <- optional x
      case e of
        Nothing -> pure ([], Just c')
        Just e' -> first ((c', e') :) <$> commaSepRest x

exprComp :: Parser ann Whitespace -> Parser ann (Expr ann)
exprComp ws =
  (\ex a ->
     case a of
       Nothing -> ex
       Just (cf, rest) ->
         Generator (_exprAnn ex) $
         Comprehension (_exprAnn ex) ex cf rest) <$>
  expr ws <*>
  optional ((,) <$> compFor <*> many (Left <$> compFor <!> Right <$> compIf))

starExpr :: Parser ann Whitespace -> Parser ann (Expr ann)
starExpr ws =
  (\(tk, sp) -> StarExpr (pyTokenAnn tk) sp) <$>
  token ws (TkStar ()) <*>
  orExpr ws

exprListComp :: Parser ann Whitespace -> Parser ann (Expr ann)
exprListComp ws =
  (\e a ->
     case a of
       Left (cf, cfs) ->
         let
           ann = _exprAnn e
         in
           Generator ann $ Comprehension ann e cf cfs
       Right (Just (c, cs)) -> Tuple (_exprAnn e) e c cs
       Right Nothing -> e) <$>
  (expr ws <!> starExpr ws) <*>
  (Left <$>
   ((,) <$>
    compFor <*>
    many (Left <$> compFor <!> Right <$> compIf)) <!>
   Right <$>
   optional
     ((,) <$>
      (snd <$> comma ws) <*>
      optional (commaSep1' ws $ expr ws <!> starExpr ws)))

orExprList :: Parser ann Whitespace -> Parser ann (Expr ann)
orExprList ws =
  (\e -> maybe e (uncurry $ Tuple (_exprAnn e) e)) <$>
  (orExpr ws <!> starExpr ws) <*>
  optional
    ((,) <$>
     (snd <$> comma ws) <*>
     optional (commaSep1' ws $ orExpr ws <!> starExpr ws))

binOp :: Parser ann (BinOp ann) -> Parser ann (Expr ann) -> Parser ann (Expr ann)
binOp op tm =
  (\t ts ->
      case ts of
        [] -> t
        _ -> foldl (\tm (o, val) -> BinOp (_exprAnn tm) tm o val) t ts) <$>
  tm <*>
  many ((,) <$> op <*> tm)

orTest :: Parser ann Whitespace -> Parser ann (Expr ann)
orTest ws = binOp orOp andTest
  where
    orOp = (\(tk, ws) -> BoolOr (pyTokenAnn tk) ws) <$> token ws (TkOr ())

    andOp = (\(tk, ws) -> BoolAnd (pyTokenAnn tk) ws) <$> token ws (TkAnd ())
    andTest = binOp andOp notTest

    notTest =
      (\(tk, s) -> Not (pyTokenAnn tk) s) <$> token ws (TkNot ()) <*> notTest <!>
      comparison

    compOp =
      (\(tk, ws) -> maybe (Is (pyTokenAnn tk) ws) (IsNot (pyTokenAnn tk) ws)) <$>
      token ws (TkIs ()) <*> optional (snd <$> token ws (TkNot ())) <!>
      (\(tk, ws) -> NotIn (pyTokenAnn tk) ws) <$>
      token ws (TkNot ()) <*>
      (snd <$> token ws (TkIn ())) <!>
      (\(tk, ws) -> In (pyTokenAnn tk) ws) <$> token ws (TkIn ()) <!>
      (\(tk, ws) -> Equals (pyTokenAnn tk) ws) <$> token ws (TkDoubleEq ()) <!>
      (\(tk, ws) -> Lt (pyTokenAnn tk) ws) <$> token ws (TkLt ()) <!>
      (\(tk, ws) -> LtEquals (pyTokenAnn tk) ws) <$> token ws (TkLte ()) <!>
      (\(tk, ws) -> Gt (pyTokenAnn tk) ws) <$> token ws (TkGt ()) <!>
      (\(tk, ws) -> GtEquals (pyTokenAnn tk) ws) <$> token ws (TkGte ()) <!>
      (\(tk, ws) -> NotEquals (pyTokenAnn tk) ws) <$> token ws (TkBangEq ())
    comparison = binOp compOp $ orExpr ws

yieldExpr :: Parser ann Whitespace -> Parser ann (Expr ann)
yieldExpr ws =
  (\(tk, s) -> either (uncurry $ YieldFrom (pyTokenAnn tk) s) (Yield (pyTokenAnn tk) s)) <$>
  token ws (TkYield ()) <*>
  (fmap Left ((,) <$> (snd <$> token ws (TkFrom ())) <*> expr ws) <!>
   (Right <$> optional (exprList ws)))

lambda :: Parser ann Whitespace -> Parser ann (Expr ann)
lambda ws =
  (\(tk, s) -> Lambda (pyTokenAnn tk) s) <$>
  token ws (TkLambda ()) <*>
  commaSep ws untypedParam <*>
  (snd <$> token ws (TkColon ())) <*>
  expr ws

lambdaNoCond :: Parser ann Whitespace -> Parser ann (Expr ann)
lambdaNoCond ws =
  (\(tk, s) -> Lambda (pyTokenAnn tk) s) <$>
  token ws (TkLambda ()) <*>
  commaSep ws untypedParam <*>
  (snd <$> token ws (TkColon ())) <*>
  exprNoCond ws

exprNoCond :: Parser ann Whitespace -> Parser ann (Expr ann)
exprNoCond ws = orTest ws <!> lambdaNoCond ws

expr :: Parser ann Whitespace -> Parser ann (Expr ann)
expr ws =
  (\a -> maybe a (\(b, c, d, e) -> Ternary (_exprAnn a) a b c d e)) <$>
  orTest ws <*>
  optional
    ((,,,) <$>
     (snd <$> token ws (TkIf ())) <*>
     orTest ws <*>
     (snd <$> token ws (TkElse ())) <*>
     expr ws)
  <!>
  lambda ws

orExpr :: Parser ann Whitespace -> Parser ann (Expr ann)
orExpr ws =
  binOp
    ((\(tk, ws) -> BitOr (pyTokenAnn tk) ws) <$> token ws (TkPipe ()))
    xorExpr
  where
    xorExpr =
      binOp
        ((\(tk, ws) -> BitXor (pyTokenAnn tk) ws) <$> token ws (TkCaret ()))
        andExpr

    andExpr =
      binOp
        ((\(tk, ws) -> BitAnd (pyTokenAnn tk) ws) <$> token ws (TkAmpersand ()))
        shiftExpr

    shiftExpr =
      binOp
        ((\(tk, ws) -> ShiftLeft (pyTokenAnn tk) ws) <$> token ws (TkShiftLeft ()) <!>
         (\(tk, ws) -> ShiftRight (pyTokenAnn tk) ws) <$> token ws (TkShiftRight ()))
        arithExpr

    arithOp =
      (\(tk, ws) -> Plus (pyTokenAnn tk) ws) <$> token ws (TkPlus ()) <!>
      (\(tk, ws) -> Minus (pyTokenAnn tk) ws) <$> token ws (TkMinus ())
    arithExpr = binOp arithOp term

    termOp =
      (\(tk, ws) -> Multiply (pyTokenAnn tk) ws) <$> token ws (TkStar ()) <!>
      (\(tk, ws) -> At (pyTokenAnn tk) ws) <$> token ws (TkAt ()) <!>
      (\(tk, ws) -> Divide (pyTokenAnn tk) ws) <$> token ws (TkSlash ()) <!>
      (\(tk, ws) -> FloorDivide (pyTokenAnn tk) ws) <$> token ws (TkDoubleSlash ()) <!>
      (\(tk, ws) -> Percent (pyTokenAnn tk) ws) <$> token ws (TkPercent ())
    term = binOp termOp factor

    factor =
      ((\(tk, s) -> let ann = pyTokenAnn tk in UnOp ann (Negate ann s)) <$>
       token ws (TkMinus ())
       <!>
       (\(tk, s) -> let ann = pyTokenAnn tk in UnOp ann (Positive ann s)) <$>
       token ws (TkPlus ())
       <!>
       (\(tk, s) -> let ann = pyTokenAnn tk in UnOp ann (Complement ann s)) <$>
       token ws (TkTilde ())) <*> factor
      <!>
      power

    powerOp = (\(tk, ws) -> Exp (pyTokenAnn tk) ws) <$> token ws (TkDoubleStar ())
    power =
      (\a -> maybe a (uncurry $ BinOp (_exprAnn a) a)) <$>
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
      (\a b c -> Deref (_exprAnn c) c a b) <$>
      (snd <$> token ws (TkDot ())) <*>
      identifier ws

      <!>

      (\a b c d -> Call (_exprAnn d) d a b c) <$>
      (snd <$> token anySpace (TkLeftParen ())) <*>
      optional (commaSep1' anySpace arg) <*>
      (snd <$> token anySpace (TkRightParen ()))

      <!>

      (\a b c d -> Subscript (_exprAnn d) d a b c) <$>
      (snd <$> token anySpace (TkLeftBracket ())) <*>
      commaSep1' anySpace subscript <*>
      (snd <$> token ws (TkRightBracket ()))

    atomExpr =
      (\(mAwait, a) b ->
         let e = foldl' (&) a b
         in maybe e (\(tk, sp) -> Await (pyTokenAnn tk) sp e) mAwait) <$>
      try ((,) <$> optional (token ws $ TkIdent "await" ()) <*> atom) <*>
      many trailer
      <!>
      foldl' (&) <$> atom <*> many trailer

    parensOrUnit =
      (\(tk, s) maybeEx sps ->
       case maybeEx of
         Nothing -> Unit (pyTokenAnn tk) s sps
         Just ex -> Parens (pyTokenAnn tk) s ex sps) <$>
      token anySpace (TkLeftParen ()) <*>
      optional (yieldExpr anySpace <!> exprListComp anySpace) <*>
      (snd <$> token ws (TkRightParen ()))

    list =
      (\(tk, sp1) ->
         maybe (List (pyTokenAnn tk) sp1 Nothing) (\f -> f (pyTokenAnn tk) sp1)) <$>
      token anySpace (TkLeftBracket ()) <*>
      optional
        ((\e a ann ws1 ->
          case a of
            Left (cf, cfs) -> ListComp ann ws1 (Comprehension (_exprAnn e) e cf cfs)
            Right Nothing -> List ann ws1 (Just $ CommaSepOne1' e Nothing)
            Right (Just (c, Nothing)) -> List ann ws1 (Just $ CommaSepOne1' e $ Just c)
            Right (Just (c, Just cs)) -> List ann ws1 (Just $ CommaSepMany1' e c cs)) <$>
        (expr anySpace <!> starExpr anySpace) <*>
        (Left <$>
        ((,) <$>
          compFor <*>
          many (Left <$> compFor <!> Right <$> compIf)) <!>
        Right <$>
        optional
          ((,) <$>
           (snd <$> comma anySpace) <*>
           optional (commaSep1' anySpace (expr anySpace <!> starExpr anySpace))))) <*>
      (snd <$> token ws (TkRightBracket()))

    doubleStarExpr ws =
      (\(tk, sp) -> DictUnpack (pyTokenAnn tk) sp) <$>
      token ws (TkDoubleStar ()) <*>
      orExpr ws

    dictItem =
      (\a -> DictItem (_exprAnn a) a) <$>
      expr anySpace <*>
      (snd <$> colon anySpace) <*>
      expr anySpace
      <!>
      doubleStarExpr anySpace

    compRHS = (,) <$> compFor <*> many (Left <$> compFor <!> Right <$> compIf)

    dictOrSet = do
      (a, ws1) <- token anySpace (TkLeftBrace ())
      let ann = pyTokenAnn a
      maybeExpr <-
        optional $
          Left . Left <$> expr anySpace <!>
          Left . Right <$> starExpr anySpace <!>
          Right <$> doubleStarExpr anySpace
      (case maybeExpr of
         Nothing -> pure $ Dict ann ws1 Nothing
         Just (Left (Left ex)) -> do
           maybeColon <- optional $ snd <$> token anySpace (TkColon ())
           case maybeColon of
             Nothing ->
               -- The order of this choice matters because commaSepRest is implemented
               -- in a slightly odd way
               (\(c, d) -> SetComp ann ws1 (Comprehension (_exprAnn ex) ex c d)) <$> compRHS
               <!>
               (\(rest, final) -> Set ann ws1 ((ex, rest, final) ^. _CommaSep1')) <$> commaSepRest (expr ws <!> starExpr ws)
             Just clws ->
               (\ex2 a ->
                 let
                   dictItemAnn = _exprAnn ex
                   firstDictItem = DictItem dictItemAnn ex clws ex2
                 in
                 case a of
                   Left (c, d) ->
                     DictComp ann ws1 (Comprehension dictItemAnn firstDictItem c d)
                   Right (rest, final) ->
                     Dict ann ws1 (Just $ (firstDictItem, rest, final) ^. _CommaSep1')) <$>
               expr anySpace <*>
               (Left <$> compRHS <!> Right <$> commaSepRest dictItem)
         Just (Left (Right ex)) ->
           ((\(c, d) -> SetComp ann ws1 (Comprehension (_exprAnn ex) ex c d)) <$> compRHS
           <!>
           (\(rest, final) -> Set ann ws1 ((ex, rest, final) ^. _CommaSep1')) <$> commaSepRest (expr ws <!> starExpr ws))
         Just (Right ex) ->
           ((\(c, d) -> DictComp ann ws1 (Comprehension (_dictItemAnn ex) ex c d)) <$> compRHS
           <!>
           (\(rest, final) -> Dict ann ws1 (Just $ (ex, rest, final) ^. _CommaSep1')) <$> commaSepRest dictItem)) <*>
         (snd <$> token ws (TkRightBrace ()))

    atom =
      dictOrSet <!>
      list <!>
      none ws <!>
      bool ws <!>
      ellipsis ws <!>
      integer ws <!>
      float ws <!>
      imag ws <!>
      stringOrBytes ws <!>
      Ident <$> identifier ws <!>
      parensOrUnit

smallStatement :: Parser ann (SmallStatement ann)
smallStatement =
  returnSt <!>
  passSt <!>
  breakSt <!>
  continueSt <!>
  globalSt <!>
  delSt <!>
  importSt <!>
  raiseSt <!>
  exprOrAssignSt <!>
  yieldSt <!>
  assertSt
  where
    assertSt =
      (\(tk, s) -> Assert (pyTokenAnn tk) s) <$>
      token space (TkAssert ()) <*>
      expr space <*>
      optional ((,) <$> (snd <$> comma space) <*> expr space)

    yieldSt = (\a -> Expr (_exprAnn a) a) <$> yieldExpr space

    returnSt =
      (\(tkReturn, retSpaces) -> Return (pyTokenAnn tkReturn) retSpaces) <$>
      token space (TkReturn ()) <*>
      optional (exprList space)

    passSt = uncurry (Pass . pyTokenAnn) <$> token space (TkPass ())
    breakSt = uncurry (Break . pyTokenAnn) <$> token space (TkBreak ())
    continueSt = uncurry (Continue . pyTokenAnn) <$> token space (TkContinue ())

    augAssign =
      (\(tk, s) -> PlusEq (pyTokenAnn tk) s) <$> token space (TkPlusEq ()) <!>
      (\(tk, s) -> MinusEq (pyTokenAnn tk) s) <$> token space (TkMinusEq ()) <!>
      (\(tk, s) -> AtEq (pyTokenAnn tk) s) <$> token space (TkAtEq ()) <!>
      (\(tk, s) -> StarEq (pyTokenAnn tk) s) <$> token space (TkStarEq ()) <!>
      (\(tk, s) -> SlashEq (pyTokenAnn tk) s) <$> token space (TkSlashEq ()) <!>
      (\(tk, s) -> PercentEq (pyTokenAnn tk) s) <$> token space (TkPercentEq ()) <!>
      (\(tk, s) -> AmpersandEq (pyTokenAnn tk) s) <$> token space (TkAmpersandEq ()) <!>
      (\(tk, s) -> PipeEq (pyTokenAnn tk) s) <$> token space (TkPipeEq ()) <!>
      (\(tk, s) -> CaretEq (pyTokenAnn tk) s) <$> token space (TkCaretEq ()) <!>
      (\(tk, s) -> ShiftLeftEq (pyTokenAnn tk) s) <$> token space (TkShiftLeftEq ()) <!>
      (\(tk, s) -> ShiftRightEq (pyTokenAnn tk) s) <$> token space (TkShiftRightEq ()) <!>
      (\(tk, s) -> DoubleStarEq (pyTokenAnn tk) s) <$> token space (TkDoubleStarEq ()) <!>
      (\(tk, s) -> DoubleSlashEq (pyTokenAnn tk) s) <$> token space (TkDoubleSlashEq ())

    exprOrAssignSt =
      (\a ->
         maybe
           (Expr (_exprAnn a) a)
           (either
              (Assign (_exprAnn a) a)
              (uncurry $ AugAssign (_exprAnn a) a))) <$>
      exprList space <*>
      optional
        (Left <$> some1 ((,) <$> (snd <$> token space (TkEq ())) <*> (yieldExpr space <!> exprList space)) <!>
         Right <$> ((,) <$> augAssign <*> (yieldExpr space <!> exprList space)))

    globalSt =
      (\(tk, s) -> Global (pyTokenAnn tk) $ NonEmpty.fromList s) <$>
      token space (TkGlobal ()) <*>
      commaSep1 space (identifier space)

    delSt =
      (\(tk, s) -> Del (pyTokenAnn tk) $ NonEmpty.fromList s) <$>
      token space (TkDel ()) <*>
      commaSep1' space (orExpr space)

    raiseSt =
      (\(tk, s) -> Raise (pyTokenAnn tk) s) <$>
      token space (TkRaise ()) <*>
      optional
        ((,) <$>
         expr space <*>
         optional
           ((,) <$>
            (snd <$> token space (TkFrom ())) <*>
            expr space))

    importSt = importName <!> importFrom
      where
        moduleName =
          makeModuleName <$>
          identifier space <*>
          many
            ((,) <$>
             (snd <$> token space (TkDot ())) <*>
             identifier space)

        importAs ws getAnn p =
          (\a -> ImportAs (getAnn a) a) <$>
          p <*>
          optional
            ((,) <$>
             (NonEmpty.fromList . snd <$> token ws (TkAs ())) <*>
             identifier ws)

        importName =
          (\(tk, s) -> Import (pyTokenAnn tk) $ NonEmpty.fromList s) <$>
          token space (TkImport ()) <*>
          commaSep1 space (importAs space _moduleNameAnn moduleName)

        dots =
          fmap concat . some $
          pure . Dot . snd <$> token space (TkDot ()) <!>
          (\(_, ws) -> [Dot [], Dot [], Dot ws]) <$> token space (TkEllipsis ())

        relativeModuleName =
          RelativeWithName [] <$> moduleName

          <!>

          (\a -> maybe (Relative $ NonEmpty.fromList a) (RelativeWithName a)) <$>
          dots <*>
          optional moduleName

        importTargets =
          (\(tk, s) -> ImportAll (pyTokenAnn tk) s) <$>
          token space (TkStar ())

          <!>

          (\(tk, s) -> ImportSomeParens (pyTokenAnn tk) s) <$>
          token anySpace (TkLeftParen ()) <*>
          commaSep1' anySpace (importAs anySpace _identAnn (identifier anySpace)) <*>
          (snd <$> token space (TkRightParen ()))

          <!>

          (\a -> ImportSome (importAsAnn $ commaSep1Head a) a) <$>
          commaSep1 space (importAs space _identAnn (identifier space))

        importFrom =
          (\(tk, s) -> From (pyTokenAnn tk) s) <$>
          token space (TkFrom ()) <*>
          relativeModuleName <*>
          (snd <$> token space (TkImport ())) <*>
          importTargets

sepBy1' :: Parser ann a -> Parser ann sep -> Parser ann (a, [(sep, a)], Maybe sep)
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

statement :: Parser ann (Statement ann)
statement =
  -- It's important to parse compound statements first, because the 'async' keyword
  -- is actually an identifier and we'll have to bactrack
  CompoundStatement <$> compoundStatement

  <!>

  (\d (a, b, c) -> SmallStatements d a b c) <$>
  indents <*>
  sepBy1' smallStatement (snd <$> semicolon space) <*>
  (Right <$> try eol <!> Left <$> optional comment <* eof)

suite :: Parser ann (Suite ann)
suite =
  (\(tk, s) ->
     either
       (uncurry $ SuiteOne (pyTokenAnn tk) s)
       (uncurry $ SuiteMany (pyTokenAnn tk) s)) <$>
  colon space <*>
  ((fmap Left $
    (,) <$>
    smallStatement <*>
    eol)
    <!>
   (fmap Right $
    (,) <$>
    eol <*>
    fmap Block
      (flip (foldr NonEmpty.cons) <$>
        commentOrIndent <*>
        some1 line) <*
    dedent))
  where
    commentOrEmpty =
      (,) <$>
      (foldOf (indentsValue.folded.indentWhitespaces) <$> indents) <*>
      eol

    commentOrIndent = many (Left <$> commentOrEmpty) <* indent

    line = Left <$> commentOrEmpty <!> Right <$> statement

comma :: Parser ann Whitespace -> Parser ann (PyToken ann, [Whitespace])
comma ws = token ws $ TkComma ()

colon :: Parser ann Whitespace -> Parser ann (PyToken ann, [Whitespace])
colon ws = token ws $ TkColon ()

semicolon :: Parser ann Whitespace -> Parser ann (PyToken ann, [Whitespace])
semicolon ws = token ws $ TkSemicolon ()

commaSep :: Parser ann Whitespace -> Parser ann a -> Parser ann (CommaSep a)
commaSep ws pa =
  (\a -> maybe (CommaSepOne a) (uncurry $ CommaSepMany a)) <$>
  pa <*>
  optional ((,) <$> (snd <$> comma ws) <*> commaSep ws pa)

  <!>

  pure CommaSepNone

commaSep1 :: Parser ann Whitespace -> Parser ann a -> Parser ann (CommaSep1 a)
commaSep1 ws val = go
  where
    go =
      (\a -> maybe (CommaSepOne1 a) (uncurry $ CommaSepMany1 a)) <$>
      val <*>
      optional ((,) <$> (snd <$> comma ws) <*> go)

commaSep1' :: Parser ann Whitespace -> Parser ann a -> Parser ann (CommaSep1' a)
commaSep1' ws pa =
  (\(a, b, c) -> from a b c) <$> sepBy1' pa (snd <$> comma ws)
  where
    from a [] b = CommaSepOne1' a b
    from a ((b, c) : bs) d = CommaSepMany1' a b $ from c bs d

untypedParam :: Parser ann (Param ann)
untypedParam =
  (\a b ->
     maybe
       (PositionalParam (_identAnn a) a b)
       (uncurry $ KeywordParam (_identAnn a) a b)) <$>
  identifier anySpace <*>
  pure Nothing <*>
  optional ((,) <$> (snd <$> token anySpace (TkEq ())) <*> expr anySpace)

  <!>

  (\(a, b) -> StarParam (pyTokenAnn a) b) <$>
  token anySpace (TkStar ()) <*>
  optional (identifier anySpace) <*>
  pure Nothing

  <!>

  (\(a, b) -> DoubleStarParam (pyTokenAnn a) b) <$>
  token anySpace (TkDoubleStar ()) <*>
  identifier anySpace <*>
  pure Nothing

typedParam :: Parser ann (Param ann)
typedParam =
  (\a b ->
     maybe
       (PositionalParam (_identAnn a) a b)
       (uncurry $ KeywordParam (_identAnn a) a b)) <$>
  identifier anySpace <*>
  optional tyAnn <*>
  optional ((,) <$> (snd <$> token anySpace (TkEq ())) <*> expr anySpace)

  <!>

  (\(a, b) ->
     maybe
       (StarParam (pyTokenAnn a) b Nothing Nothing)
       (\(c, d) -> StarParam (pyTokenAnn a) b (Just c) d)) <$>
  token anySpace (TkStar ()) <*>
  optional ((,) <$> identifier anySpace <*> optional tyAnn)

  <!>

  (\(a, b) -> DoubleStarParam (pyTokenAnn a) b) <$>
  token anySpace (TkDoubleStar ()) <*>
  identifier anySpace <*>
  optional tyAnn
  where
    tyAnn =
      (,) <$> (snd <$> token anySpace (TkColon ())) <*> expr anySpace

arg :: Parser ann (Arg ann)
arg =
  (do
      e <- exprComp anySpace
      case e of
        Ident ident -> do
          eqSpaces <- optional $ snd <$> token anySpace (TkEq ())
          case eqSpaces of
            Nothing -> pure $ PositionalArg (_exprAnn e) e
            Just s -> KeywordArg (_exprAnn e) ident s <$> expr anySpace
        _ -> pure $ PositionalArg (_exprAnn e) e)

  <!>

  (\a -> PositionalArg (_exprAnn a) a) <$> expr anySpace

  <!>

  (\(a, b) -> StarArg (pyTokenAnn a) b) <$> token anySpace (TkStar ()) <*> expr anySpace

  <!>

  (\(a, b) -> DoubleStarArg (pyTokenAnn a) b) <$>
  token anySpace (TkDoubleStar ()) <*>
  expr anySpace

decoratorValue :: Parser ann (Expr ann)
decoratorValue = do
  id1 <- identifier space
  ids <- many ((,) <$> (snd <$> token space (TkDot ())) <*> identifier space)
  args <-
    optional $
    (,,) <$>
    (snd <$> token anySpace (TkLeftParen ())) <*>
    optional (commaSep1' anySpace arg) <*>
    (snd <$> token space (TkRightParen ()))
  let
    derefs =
      foldl
        (\b (ws, a) -> Deref (_exprAnn b) b ws a)
        (Ident id1)
        ids
  pure $
    case args of
      Nothing -> derefs
      Just (l, x, r) -> Call (_exprAnn derefs) derefs l x r

decorator :: Parser ann (Decorator ann)
decorator =
  (\i (tk, spcs) -> Decorator (pyTokenAnn tk) i spcs) <$>
  indents <*>
  token space (TkAt ()) <*>
  decoratorValue <*>
  eol

compoundStatement :: Parser ann (CompoundStatement ann)
compoundStatement =
  ifSt <!>
  whileSt <!>
  trySt <!>
  decorated <!>
  asyncSt <!>
  classSt [] <!>
  fundef Nothing [] <!>
  withSt Nothing <!>
  forSt Nothing
  where
    decorated = do
      d <- some decorator
      (fundef (Just . optional $ token space (TkIdent "async" ())) d) <!> classSt d

    classSt d =
      (\a (tk, s) -> ClassDef (pyTokenAnn tk) d a $ NonEmpty.fromList s) <$>
      indents <*>
      token space (TkClass ()) <*>
      identifier space <*>
      optional
        ((,,) <$>
         (snd <$> token anySpace (TkLeftParen ())) <*>
         optional (commaSep1' anySpace arg) <*>
         (snd <$> token space (TkRightParen ()))) <*>
      suite

    ifSt =
      (\a (tk, s) -> If (pyTokenAnn tk) a s) <$>
      indents <*>
      token space (TkIf ()) <*>
      expr space <*>
      suite <*>
      many
        ((,,,) <$>
         indents <*>
         (snd <$> token space (TkElif ())) <*>
         expr space <*>
         suite) <*>
      optional
        ((,,) <$>
         indents <*>
         (snd <$> token space (TkElse ())) <*>
         suite)

    whileSt =
      (\a (tk, s) -> While (pyTokenAnn tk) a s) <$>
      indents <*>
      token space (TkWhile ()) <*>
      expr space <*>
      suite

    exceptAs =
      (\a -> ExceptAs (_exprAnn a) a) <$>
      expr space <*>
      optional ((,) <$> (snd <$> token space (TkAs())) <*> identifier space)

    trySt =
      (\i (tk, s) a d ->
         case d of
           Left (e, f, g) -> TryFinally (pyTokenAnn tk) i s a e f g
           Right (e, f, g) -> TryExcept (pyTokenAnn tk) i s a e f g) <$>
      indents <*>
      token space (TkTry ()) <*>
      suite <*>
      (fmap Left
         ((,,) <$>
          indents <*>
          (snd <$> token space (TkFinally ())) <*>
          suite)

        <!>

        fmap Right
          ((,,) <$>
           some1
             ((,,,) <$>
              indents <*>
              (snd <$> token space (TkExcept ())) <*>
              optional exceptAs <*>
              suite) <*>
           optional
             ((,,) <$>
              indents <*>
              (snd <$> token space (TkElse ())) <*>
              suite) <*>
           optional
             ((,,) <$>
              indents <*>
              (snd <$> token space (TkFinally ())) <*>
              suite)))

    doAsync = Just <$> token space (TkIdent "async" ())
    asyncSt =
      fundef (Just doAsync) [] <!>
      withSt (Just doAsync) <!>
      forSt (Just doAsync)

    fundef pAsync d =
      try
        ((\a async (tkDef, defSpaces) ->
           Fundef
             (maybe (pyTokenAnn tkDef) (pyTokenAnn . fst) async)
             d
             a
             (NonEmpty.fromList . snd <$> async)
             (NonEmpty.fromList defSpaces)) <$>
         indents <*>
         fromMaybe (pure Nothing) pAsync <*>
         token space (TkDef ())) <*>
      identifier space <*>
      fmap snd (token anySpace $ TkLeftParen ()) <*>
      commaSep anySpace typedParam <*>
      fmap snd (token space $ TkRightParen ()) <*>
      optional ((,) <$> (snd <$> token space (TkRightArrow ())) <*> expr space) <*>
      suite

    withSt pAsync =
      try
        ((\a async (tk, s) ->
           With
             (maybe (pyTokenAnn tk) (pyTokenAnn . fst) async)
             a
             (NonEmpty.fromList . snd <$> async)
             s) <$>
         indents <*>
         fromMaybe (pure Nothing) pAsync <*>
         token space (TkWith ())) <*>
      commaSep1
        space
        ((\a -> WithItem (_exprAnn a) a) <$>
         expr space <*>
         optional ((,) <$> (snd <$> token space (TkAs ())) <*> orExpr space)) <*>
      suite

    forSt pAsync =
      try
        ((\a async (tk, s) ->
           For
             (maybe (pyTokenAnn tk) (pyTokenAnn . fst) async)
             a
             (NonEmpty.fromList . snd <$> async)
             s) <$>
         indents <*>
         fromMaybe (pure Nothing) pAsync <*>
         token space (TkFor ())) <*>
      orExprList space <*>
      (snd <$> token space (TkIn ())) <*>
      exprList space <*>
      suite <*>
      optional
        ((,,) <$>
         indents <*>
         (snd <$> token space (TkElse ())) <*>
         suite)

module_ :: Parser ann (Module ann)
module_ =
  Module <$>
  many (Left <$> maybeComment <!> Right <$> statement)
  where
    maybeComment =
      (\ws cmt nl -> (ws, cmt, nl)) <$>
      indents <*>
      optional comment <*>
      (Just <$> eol <!> Nothing <$ eof)
