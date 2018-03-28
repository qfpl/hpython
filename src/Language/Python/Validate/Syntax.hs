{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
{-# language TypeSynonymInstances, FlexibleInstances #-}
{-# language TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# language RankNTypes #-}
module Language.Python.Validate.Syntax where

import Control.Applicative
import Control.Lens.Cons
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Plated
import Control.Lens.Review
import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Traversal
import Control.Monad.State
import Control.Monad.Reader
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.Functor
import Data.Functor.Compose
import Data.List
import Data.Maybe
import Data.Semigroup (Semigroup(..))
import Data.Type.Set (Nub, Member)
import Data.Validate

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Optics
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation
import Language.Python.Validate.Syntax.Error

deleteBy' :: (a -> b -> Bool) -> a -> [b] -> [b]
deleteBy' _ _ [] = []
deleteBy' eq a (b:bs) = if a `eq` b then bs else b : deleteBy' eq a bs

deleteFirstsBy' :: (a -> b -> Bool) -> [a] -> [b] -> [a]
deleteFirstsBy' eq = foldl (flip (deleteBy' (flip eq)))

data Syntax

data SyntaxContext
  = SyntaxContext
  { _inLoop :: Bool
  , _inFunction :: Maybe [String]
  }

newtype ValidateSyntax e a
  = ValidateSyntax
  { unValidateSyntax
    :: Compose
         (ReaderT SyntaxContext (State [String]))
         (Validate [e])
         a
  } deriving (Functor, Applicative)

runValidateSyntax :: SyntaxContext -> [String] -> ValidateSyntax e a -> Validate [e] a
runValidateSyntax ctxt nlscope =
  flip evalState nlscope .
  flip runReaderT ctxt . getCompose .
  unValidateSyntax

syntaxContext :: ValidateSyntax e SyntaxContext
syntaxContext =
  ValidateSyntax . Compose . fmap pure $ ask

nonlocals :: ValidateSyntax e [String]
nonlocals =
  ValidateSyntax . Compose . fmap pure $ get

bindValidateSyntax :: ValidateSyntax e a -> (a -> ValidateSyntax e b) -> ValidateSyntax e b
bindValidateSyntax v f =
  ValidateSyntax . Compose $ do
    a <- getCompose $ unValidateSyntax v
    case a of
      Failure e -> pure $ Failure e
      Success a -> getCompose . unValidateSyntax $ f a

localSyntaxContext
  :: (SyntaxContext -> SyntaxContext)
  -> ValidateSyntax e a
  -> ValidateSyntax e a
localSyntaxContext f v =
  ValidateSyntax . Compose $
  local f (getCompose $ unValidateSyntax v)

modifyNonlocals :: ([String] -> [String]) -> ValidateSyntax e ()
modifyNonlocals f =
  ValidateSyntax . Compose . fmap pure $ modify f

localNonlocals :: ([String] -> [String]) -> ValidateSyntax e a -> ValidateSyntax e a
localNonlocals f v =
  ValidateSyntax . Compose $ do
    before <- get
    modify f
    res <- getCompose $ unValidateSyntax v
    put before
    pure res

syntaxErrors :: [e] -> ValidateSyntax e a
syntaxErrors es = ValidateSyntax . Compose . pure $ Failure es

initialSyntaxContext :: SyntaxContext
initialSyntaxContext =
  SyntaxContext
  { _inLoop = False
  , _inFunction = Nothing
  }

class EndToken s where
  endToken :: s -> String

class StartToken s where
  startToken :: s -> String

isIdentifierChar :: Char -> Bool
isIdentifierChar = foldr (liftA2 (||)) (pure False) [isIdentifierStart, isDigit]

isIdentifierStart :: Char -> Bool
isIdentifierStart = foldr (liftA2 (||)) (pure False) [isLetter, (=='_')]

isIdentifier :: String -> Bool
isIdentifier s =
  case s ^? _Cons of
    Nothing -> False
    Just (x, xs) -> isIdentifierStart x && all isIdentifierChar xs

validateIdent
  :: ( AsSyntaxError e v ann
     , Member Indentation v
     )
  => Ident v ann
  -> ValidateSyntax e (Ident (Nub (Syntax ': v)) ann)
validateIdent (MkIdent a name)
  | not (all isAscii name) = syntaxErrors [_BadCharacter # (a, name)]
  | null name = syntaxErrors [_EmptyIdentifier # a]
  | name `elem` reservedWords = syntaxErrors [_IdentifierReservedWord # (a, name)]
  | otherwise = pure $ MkIdent a name

instance EndToken (BinOp a) where
  endToken Is{} = "is"
  endToken Minus{} = "-"
  endToken Exp{} = "*"
  endToken BoolAnd{} = "and"
  endToken BoolOr{} = "or"
  endToken Multiply{} = "*"
  endToken Divide{} = "/"
  endToken Plus{} = "+"
  endToken Equals{} = "="

instance StartToken (BinOp a) where
  startToken Is{} = "is"
  startToken Minus{} = "-"
  startToken Exp{} = "*"
  startToken BoolAnd{} = "and"
  startToken BoolOr{} = "or"
  startToken Multiply{} = "*"
  startToken Divide{} = "/"
  startToken Plus{} = "+"
  startToken Equals{} = "="

instance EndToken (Expr v a) where
  endToken List{} = "]"
  endToken (Deref _ _ _ _ i) = _identValue i
  endToken Call{} = ")"
  endToken None{} = "None"
  endToken (BinOp _ _ _ _ _ e) = endToken e
  endToken (Negate _ _ e) = endToken e
  endToken Parens{} = ")"
  endToken (Ident _ i) = _identValue i
  endToken (Int _ i) = show i
  endToken (Bool _ b) = show b
  endToken String{} = "\""

instance StartToken (Expr v a) where
  startToken List{} = "["
  startToken (Deref _ e _ _ _) = startToken e
  startToken (Call _ e _ _) = startToken e
  startToken None{} = "None"
  startToken (BinOp _ e _ _ _ _) = startToken e
  startToken Negate{} = "-"
  startToken Parens{} = "("
  startToken (Ident _ i) = _identValue i
  startToken (Int _ i) = show i
  startToken (Bool _ b) = show b
  startToken String{} = "\""

instance EndToken String where
  endToken s = fromMaybe s (words s ^? _last)

instance StartToken String where
  startToken s = fromMaybe s (words s ^? _head)

validateWhitespace
  :: ( EndToken x, StartToken y
     , AsSyntaxError e v a
     )
  => a
  -> (x, x -> String)
  -> [Whitespace]
  -> (y, y -> String)
  -> ValidateSyntax e [Whitespace]
validateWhitespace ann (a, aStr) [] (b, bStr)
  | isIdentifier (endToken a ++ startToken b)
  = syntaxErrors [_MissingSpacesIn # (ann, aStr a, bStr b)]
validateWhitespace _ _ ws _ = pure ws

validateExprSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Expr v a
  -> ValidateSyntax e (Expr (Nub (Syntax ': v)) a)
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
  => Block v a
  -> ValidateSyntax e (Block (Nub (Syntax ': v)) a)
validateBlockSyntax (Block bs) = Block . NonEmpty.fromList <$> go (NonEmpty.toList bs)
  where
    go [] = error "impossible"
    go [b] = pure <$> traverseOf _3 validateStatementSyntax b
    go (b:bs) = (:) <$> traverseOf _3 validateStatementSyntax b <*> go bs

validateCompoundStatementSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => CompoundStatement v a
  -> ValidateSyntax e (CompoundStatement (Nub (Syntax ': v)) a)
validateCompoundStatementSyntax (Fundef a ws1 name ws2 params ws3 ws4 nl body) =
  let
    paramIdents = params ^.. folded.unvalidated.paramName.identValue
  in
    Fundef a ws1 <$>
    validateIdent name <*>
    pure ws2 <*>
    validateParamsSyntax params <*>
    pure ws3 <*>
    pure ws4 <*>
    pure nl <*>
    localNonlocals id
      (localSyntaxContext
         (\ctxt ->
            ctxt
            { _inLoop = False
            , _inFunction =
                fmap
                  (`union` paramIdents)
                  (_inFunction ctxt) <|>
                Just paramIdents
            })
         (validateBlockSyntax body))
validateCompoundStatementSyntax (If a ws1 expr ws2 ws3 nl body body') =
  If a <$>
  validateWhitespace a ("if", id) ws1 (expr, renderExpr) <*>
  validateExprSyntax expr <*>
  pure ws2 <*>
  pure ws3 <*>
  pure nl <*>
  validateBlockSyntax body <*>
  traverseOf (traverse._4) validateBlockSyntax body'
validateCompoundStatementSyntax (While a ws1 expr ws2 ws3 nl body) =
  While a <$>
  validateWhitespace a ("while", id) ws1 (expr, renderExpr) <*>
  validateExprSyntax expr <*>
  pure ws2 <*>
  pure ws3 <*>
  pure nl <*>
  localSyntaxContext (\ctxt -> ctxt { _inLoop = True}) (validateBlockSyntax body)

validateSmallStatementSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => SmallStatement v a
  -> ValidateSyntax e (SmallStatement (Nub (Syntax ': v)) a)
validateSmallStatementSyntax (Return a ws expr) =
  syntaxContext `bindValidateSyntax` \sctxt ->
    case _inFunction sctxt of
      Just{} ->
        Return a <$>
        validateWhitespace a ("return", id) ws (expr, renderExpr) <*>
        validateExprSyntax expr
      _ -> syntaxErrors [_ReturnOutsideFunction # a]
validateSmallStatementSyntax (Expr a expr) =
  Expr a <$>
  validateExprSyntax expr
validateSmallStatementSyntax (Assign a lvalue ws1 ws2 rvalue) =
  syntaxContext `bindValidateSyntax` \sctxt ->
    let
      assigns =
        if isJust (_inFunction sctxt)
        then lvalue ^.. unvalidated.cosmos._Ident._2.identValue
        else []
    in
      (Assign a <$>
      (if canAssignTo lvalue
        then validateExprSyntax lvalue
        else syntaxErrors [_CannotAssignTo # (a, lvalue)]) <*>
      pure ws1 <*>
      pure ws2 <*>
      validateExprSyntax rvalue) <*
      modifyNonlocals (assigns ++)
validateSmallStatementSyntax p@Pass{} = pure $ coerce p
validateSmallStatementSyntax (Break a) =
  syntaxContext `bindValidateSyntax` \sctxt ->
    if _inLoop sctxt
    then pure $ Break a
    else syntaxErrors [_BreakOutsideLoop # a]
validateSmallStatementSyntax (Global a ws ids) =
  Global a ws <$> traverse validateIdent ids
validateSmallStatementSyntax (Nonlocal a ws ids) =
  syntaxContext `bindValidateSyntax` \sctxt ->
  nonlocals `bindValidateSyntax` \nls ->
  (case deleteFirstsBy' (\a -> (==) (a ^. unvalidated.identValue)) (ids ^.. folded) nls of
     [] -> pure ()
     ids -> traverse_ (\e -> syntaxErrors [_NoBindingNonlocal # e]) ids) *>
  case _inFunction sctxt of
    Nothing -> syntaxErrors [_NonlocalOutsideFunction # a]
    Just params ->
      case intersect params (ids ^.. folded.unvalidated.identValue) of
        [] -> Nonlocal a ws <$> traverse validateIdent ids
        bad -> syntaxErrors [_ParametersNonlocal # (a, bad)]
validateSmallStatementSyntax (Del a ws ids) =
  Del a ws <$> traverse validateIdent ids

validateStatementSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Statement v a
  -> ValidateSyntax e (Statement (Nub (Syntax ': v)) a)
validateStatementSyntax (CompoundStatement c) =
  CompoundStatement <$> validateCompoundStatementSyntax c
validateStatementSyntax (SmallStatements s ss sc nl) =
  SmallStatements <$>
  validateSmallStatementSyntax s <*>
  traverseOf (traverse._3) validateSmallStatementSyntax ss <*>
  pure sc <*>
  pure nl

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
  => CommaSep (Arg v a)
  -> ValidateSyntax e (CommaSep (Arg (Nub (Syntax ': v)) a))
validateArgsSyntax e = go [] False (toList e) $> coerce e
  where
    go
      :: (AsSyntaxError e v a, Member Indentation v)
      => [String]
      -> Bool
      -> [Arg v a]
      -> ValidateSyntax e [Arg (Nub (Syntax ': v)) a]
    go _ _ [] = pure []
    go names False (PositionalArg a expr : args) =
      liftA2 (:)
        (PositionalArg a <$> validateExprSyntax expr)
        (go names False args)
    go names True (PositionalArg a expr : args) =
      syntaxErrors [_PositionalAfterKeywordArg # (a, expr)] <*>
      go names True args
    go names _ (KeywordArg a name ws1 ws2 expr : args)
      | _identValue name `elem` names =
          syntaxErrors [_DuplicateArgument # (a, _identValue name)] <*>
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
  => CommaSep (Param v a)
  -> ValidateSyntax e (CommaSep (Param (Nub (Syntax ': v)) a))
validateParamsSyntax e = go [] False (toList e) $> coerce e
  where
    go _ _ [] = pure []
    go names False (PositionalParam a name : params)
      | _identValue name `elem` names =
          syntaxErrors [_DuplicateArgument # (a, _identValue name)] <*>
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
        syntaxErrors errs <*> go (name':names) True params
    go names _ (KeywordParam a name ws1 ws2 expr : params)
      | _identValue name `elem` names =
          syntaxErrors [_DuplicateArgument # (a, _identValue name)] <*> go names True params
      | otherwise =
          liftA2 (:)
            (KeywordParam a <$>
             validateIdent name <*>
             pure ws1 <*>
             pure ws2 <*>
             validateExprSyntax expr)
            (go (_identValue name:names) True params)

makeWrapped ''ValidateSyntax
