{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
{-# language TypeSynonymInstances, FlexibleInstances #-}
module Language.Python.Validate.Syntax where

import Control.Applicative
import Control.Lens ((#), (^.), _head, _last)
import Control.Lens.Fold
import Control.Lens.Tuple
import Control.Lens.Traversal
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.Functor
import Data.Semigroup (Semigroup(..))
import Data.Type.Set
import Data.Validate

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Render
import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation
import Language.Python.Validate.Syntax.Error

data Syntax

data SyntaxContext
  = SyntaxContext
  { _inLoop :: Bool
  , _inFunction :: Bool
  }

class StartsWith s where
  startsWith :: s -> Maybe Char

class EndsWith s where
  endsWith :: s -> Maybe Char

isIdentifierChar :: Char -> Bool
isIdentifierChar = foldr (liftA2 (||)) (pure False) [isIdentifierStart, isDigit]

isIdentifierStart :: Char -> Bool
isIdentifierStart = foldr (liftA2 (||)) (pure False) [isLetter, (=='_')]

validateIdent
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Ident v a
  -> Validate [e] (Ident (Nub (Syntax ': v)) a)
validateIdent (MkIdent a name)
  | not (all isAscii name) = Failure [_BadCharacter # (a, name)]
  | null name = Failure [_EmptyIdentifier # a]
  | name `elem` reservedWords = Failure [_IdentifierReservedWord # (a, name)]
  | otherwise = Success $ MkIdent a name

instance StartsWith (BinOp a) where
  startsWith Is{} = Just 'i'
  startsWith Minus{} = Just '-'
  startsWith Exp{} = Just '*'
  startsWith BoolAnd{} = Just 'a'
  startsWith BoolOr{} = Just 'o'
  startsWith Multiply{} = Just '*'
  startsWith Divide{} = Just '/'
  startsWith Plus{} = Just '+'
  startsWith Equals{} = Just '='

instance EndsWith (BinOp a) where
  endsWith Is{} = Just 's'
  endsWith Minus{} = Just '-'
  endsWith Exp{} = Just '*'
  endsWith BoolAnd{} = Just 'd'
  endsWith BoolOr{} = Just 'r'
  endsWith Multiply{} = Just '*'
  endsWith Divide{} = Just '/'
  endsWith Plus{} = Just '+'
  endsWith Equals{} = Just '='

instance StartsWith (Expr v a) where
  startsWith List{} = Just '['
  startsWith (Deref _ e _ _ _) = startsWith e
  startsWith (Call _ e _ _) = startsWith e
  startsWith None{} = Just 'N'
  startsWith (BinOp _ e _ _ _ _) = startsWith e
  startsWith Negate{} = Just '-'
  startsWith Parens{} = Just '('
  startsWith (Ident _ s) = _identValue s ^? _head
  startsWith (Int _ i) = show i ^? _head
  startsWith (Bool _ b) = show b ^? _head
  startsWith String{} = Just '"'

instance EndsWith (Expr v a) where
  endsWith List{} = Just ']'
  endsWith (Deref _ _ _ _ s) =
    case _identValue s of
      [] -> Just '.'
      s' -> s' ^? _last
  endsWith Call{} = Just ')'
  endsWith None{} = Just 'e'
  endsWith (BinOp _ _ _ _ _ e) = endsWith e
  endsWith (Negate _ _ e) = endsWith e
  endsWith Parens{} = Just ')'
  endsWith (Ident _ s) = _identValue s ^? _last
  endsWith (Int _ i) = show i ^? _last
  endsWith (Bool _ b) = show b ^? _last
  endsWith String{} = Just '"'

instance StartsWith String where
  startsWith a = a ^? _head

instance EndsWith String where
  endsWith a = a ^? _last

validateWhitespace
  :: ( EndsWith x, StartsWith y
     , AsSyntaxError e v a
     )
  => a
  -> (x, x -> String)
  -> [Whitespace]
  -> (y, y -> String)
  -> Validate [e] [Whitespace]
validateWhitespace ann (a, aStr) [] (b, bStr)
  | Just c2 <- endsWith a
  , Just c3 <- startsWith b
  , isIdentifierChar c2
  , isIdentifierChar c3
  = Failure [_MissingSpacesIn # (ann, aStr a, bStr b)]
validateWhitespace _ _ ws _ = Success ws

validateExprSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Expr v a
  -> Validate [e] (Expr (Nub (Syntax ': v)) a)
validateExprSyntax (Parens a ws1 e ws2) = Parens a ws1 <$> validateExprSyntax e <*> pure ws2
validateExprSyntax (Bool a b) = pure $ Bool a b
validateExprSyntax (Negate a ws expr) = Negate a ws <$> validateExprSyntax expr
validateExprSyntax (String a b) = pure $ String a b
validateExprSyntax (Int a n) = pure $ Int a n
validateExprSyntax (Ident a name) = Ident a <$> validateIdent name
validateExprSyntax (List a ws1 exprs ws2) =
  List a ws1 <$> traverse validateExprSyntax exprs <*> pure ws2
validateExprSyntax (Deref a expr ws1 ws2 name) =
  Deref a <$>
  validateExprSyntax expr <*>
  pure ws1 <*>
  pure ws2 <*>
  validateIdent name
validateExprSyntax (Call a expr ws args) =
  Call a <$>
  validateExprSyntax expr <*>
  pure ws <*>
  validateArgsSyntax args
validateExprSyntax (None a) = pure $ None a
validateExprSyntax e@(BinOp a e1 ws1 op ws2 e2) =
  BinOp a <$>
  validateExprSyntax e1 <*>
  (if shouldBracketLeft op e1
   then pure ws1
   else validateWhitespace a (e1, renderExpr) ws1 (op, renderBinOp)) <*>
  pure op <*>
  (if shouldBracketRight op e2
   then pure ws2
   else validateWhitespace a (op, renderBinOp) ws2 (e2, renderExpr)) <*>
  validateExprSyntax e2

validateBlockSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => SyntaxContext
  -> Block v a
  -> Validate [e] (Block (Nub (Syntax ': v)) a)
validateBlockSyntax ctxt (Block bs) = Block . NonEmpty.fromList <$> go (NonEmpty.toList bs)
  where
    go [] = error "impossible"
    go [b] = pure <$> traverseOf _3 (validateStatementSyntax ctxt) b
    go (b:bs) =
      (:) <$>
      (case b ^. _4 of
        Nothing -> Failure [_ExpectedNewlineAfter # b]
        Just{} -> traverseOf _3 (validateStatementSyntax ctxt) b) <*>
      (go bs)

validateStatementSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => SyntaxContext
  -> Statement v a
  -> Validate [e] (Statement (Nub (Syntax ': v)) a)
validateStatementSyntax ctxt (Fundef a ws1 name ws2 params ws3 ws4 nl body) =
  Fundef a ws1 <$>
  validateIdent name <*>
  pure ws2 <*>
  validateParamsSyntax params <*>
  pure ws3 <*>
  pure ws4 <*>
  pure nl <*>
  validateBlockSyntax (ctxt { _inFunction = True}) body
validateStatementSyntax ctxt (Return a ws expr)
  | _inFunction ctxt =
      Return a <$>
      validateWhitespace a ("return", id) ws (expr, renderExpr) <*>
      validateExprSyntax expr
  | otherwise = Failure [_ReturnOutsideFunction # a]
validateStatementSyntax ctxt (Expr a expr) =
  Expr a <$>
  validateExprSyntax expr
validateStatementSyntax ctxt (If a ws1 expr ws2 ws3 nl body body') =
  If a <$>
  validateWhitespace a ("if", id) ws1 (expr, renderExpr) <*>
  validateExprSyntax expr <*>
  pure ws2 <*>
  pure ws3 <*>
  pure nl <*>
  validateBlockSyntax ctxt body <*>
  traverseOf (traverse._4) (validateBlockSyntax ctxt) body'
validateStatementSyntax ctxt (While a ws1 expr ws2 ws3 nl body) =
  While a <$>
  validateWhitespace a ("while", id) ws1 (expr, renderExpr) <*>
  validateExprSyntax expr <*>
  pure ws2 <*>
  pure ws3 <*>
  pure nl <*>
  validateBlockSyntax (ctxt { _inLoop = True}) body
validateStatementSyntax ctxt (Assign a lvalue ws1 ws2 rvalue) =
  Assign a <$>
  (if canAssignTo lvalue
   then validateExprSyntax lvalue
   else Failure [_CannotAssignTo # (a, lvalue)]) <*>
  pure ws1 <*>
  pure ws2 <*>
  validateExprSyntax rvalue
validateStatementSyntax ctxt p@Pass{} = pure $ coerce p
validateStatementSyntax ctxt (Break a)
  | _inLoop ctxt = pure $ Break a
  | otherwise = Failure [_BreakOutsideLoop # a]
validateStatementSyntax ctxt (Global a ws ids) =
  Global a ws <$> traverse validateIdent ids
validateStatementSyntax ctxt (Nonlocal a ws ids) =
  Nonlocal a ws <$> traverse validateIdent ids

canAssignTo :: Expr v a -> Bool
canAssignTo None{} = False
canAssignTo Negate{} = False
canAssignTo Int{} = False
canAssignTo Call{} = False
canAssignTo BinOp{} = False
canAssignTo Bool{} = False
canAssignTo (Parens _ _ a _) = canAssignTo a
canAssignTo String{} = False
canAssignTo (List _ _ a _) = all canAssignTo a
canAssignTo _ = True

validateArgsSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => CommaSep (Arg v a) -> Validate [e] (CommaSep (Arg (Nub (Syntax ': v)) a))
validateArgsSyntax e = go [] False (toList e) $> coerce e
  where
    go
      :: (AsSyntaxError e v a, Member Indentation v)
      => [String]
      -> Bool
      -> [Arg v a]
      -> Validate [e] [Arg (Nub (Syntax ': v)) a]
    go _ _ [] = pure []
    go names False (PositionalArg a expr : args) =
      liftA2 (:)
        (PositionalArg a <$> validateExprSyntax expr)
        (go names False args)
    go names True (PositionalArg a expr : args) =
      let
        errs = [_PositionalAfterKeywordArg # (a, expr)]
      in
        Failure errs <*> go names True args
    go names _ (KeywordArg a name ws1 ws2 expr : args)
      | _identValue name `elem` names =
          Failure [_DuplicateArgument # (a, _identValue name)] <*>
          validateIdent name <*>
          go names True args
      | otherwise =
          liftA2 (:)
            (KeywordArg a <$>
             validateIdent name <*>
             pure ws1 <*>
             pure ws2 <*>
             validateExprSyntax expr)
            (go (_identValue name:names) True args)

validateParamsSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => CommaSep (Param v a) -> Validate [e] (CommaSep (Param (Nub (Syntax ': v)) a))
validateParamsSyntax e = go [] False (toList e) $> coerce e
  where
    go _ _ [] = pure []
    go names False (PositionalParam a name : params)
      | _identValue name `elem` names =
          Failure [_DuplicateArgument # (a, _identValue name)] <*>
          validateIdent name <*>
          go (_identValue name:names) False params
      | otherwise =
          liftA2
            (:)
            (PositionalParam a <$> validateIdent name)
            (go (_identValue name:names) False params)
    go names True (PositionalParam a name : params) =
      let
        name' = _identValue name
        errs =
            [_DuplicateArgument # (a, name') | name' `elem` names] <>
            [_PositionalAfterKeywordParam # (a, name')]
      in
        Failure errs <*> go (name':names) True params
    go names _ (KeywordParam a name ws1 ws2 expr : params)
      | _identValue name `elem` names =
          Failure [_DuplicateArgument # (a, _identValue name)] <*> go names True params
      | otherwise =
          liftA2 (:)
            (KeywordParam a <$>
             validateIdent name <*>
             pure ws1 <*>
             pure ws2 <*>
             validateExprSyntax expr)
            (go (_identValue name:names) True params)
