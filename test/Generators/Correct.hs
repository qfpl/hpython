{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
module Generators.Correct where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Plated
import Control.Lens.Tuple
import Control.Monad.State
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Stack

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax
import Language.Python.Validate.Syntax

import Generators.Common

genIdent :: MonadGen m => m (Ident '[] ())
genIdent =
  Gen.filter (\i -> _identValue i `notElem` reservedWords) $
  MkIdent () <$>
  liftA2 (:)
    (Gen.choice [Gen.alpha, pure '_'])
    (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_']))

genInt :: MonadGen m => m (Expr '[] ())
genInt = Int () <$> Gen.integral (Range.constant 0 (2^32))

genBlock :: (MonadGen m, MonadState [String] m) => SyntaxContext -> m (Block '[] ())
genBlock ctxt = do
  indent <- NonEmpty.toList <$> genWhitespaces1
  go indent
  where
    go indent =
      Gen.sized $ \n ->
      if n <= 1
        then do
          s1 <- genStatement ctxt
          Block <$>
            liftA2 (:|)
              ((,,,) () indent <$> pure s1 <*> fmap Just genNewline)
              (pure [])
        else do
          n' <- Gen.integral (Range.constant 1 (n-1))
          s1 <- Gen.resize n' (genStatement ctxt)
          let n'' = n - n'
          b <- Gen.resize n'' (go indent)
          Block <$>
            liftA2 NonEmpty.cons
              ((,,,) () indent <$> pure s1 <*> fmap Just genNewline)
              (pure $ unBlock b)

genPositionalArg :: MonadGen m => m (Arg '[] ())
genPositionalArg = Gen.scale (max 0 . subtract 1) $ PositionalArg () <$> genExpr

genKeywordArg :: MonadGen m => m (Arg '[] ())
genKeywordArg =
  Gen.scale (max 0 . subtract 1) $
  KeywordArg () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genExpr

genArgs :: MonadGen m => m (CommaSep (Arg '[] ()))
genArgs =
  Gen.sized $ \n -> do
    n1 <- Gen.integral (Range.constant 0 n)
    n2 <- Gen.integral (Range.constant 0 $ n-n1)
    let n3 = n - n1 - n2

    pargs <- Gen.resize n1 $ genSizedCommaSep genPositionalArg
    -- sargs <- Gen.resize n2 $ genSizedCommaSep genStarArg
    kwargs <- Gen.resize n3 $ genSizedCommaSep genKeywordArg

    -- appendCommaSep pargs (appendCommaSep sargs kwargs)
    pure $ appendCommaSep pargs kwargs

genPositionalParams :: MonadGen m => m (CommaSep (Param '[] ()))
genPositionalParams =
  Gen.scale (max 0 . subtract 1) $
  Gen.sized $ \n -> do
    idents <- go [] n
    pure . listToCommaSep $ PositionalParam () <$> idents
  where
    go seen 0 = pure []
    go seen n = do
      i <- Gen.filter ((`notElem` seen) . _identValue) genIdent
      (i :) <$> go (_identValue i : seen) (n-1)

genKeywordParam :: MonadGen m => [String] -> m (Param '[] ())
genKeywordParam positionals =
  Gen.scale (max 0 . subtract 1) $
  KeywordParam () <$>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent <*>
  genWhitespaces <*>
  genWhitespaces <*>
  genExpr

genParams :: MonadGen m => m (CommaSep (Param '[] ()))
genParams =
  Gen.sized $ \n -> do
    n1 <- Gen.integral (Range.constant 0 n)
    n2 <- Gen.integral (Range.constant 0 $ n-n1)
    let n3 = n - n1 - n2

    pparams <- Gen.resize n1 genPositionalParams
    -- sparams <- Gen.resize n2 $ genSizedCommaSep genStarParam
    kwparams <-
      Gen.resize n3 $
      genSizedCommaSep (genKeywordParam $ pparams ^.. folded.paramName.identValue)

    -- appendCommaSep pparams (appendCommaSep sparams kwparams)
    pure $ appendCommaSep pparams kwparams

genList :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genList genExpr' =
  Gen.shrink
    (\case
        List _ _ (CommaSepOne e) _ -> [e]
        _ -> []) $
  List () <$>
  genWhitespaces <*>
  genSizedCommaSep genExpr' <*>
  genWhitespaces

genParens :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genParens genExpr' = Parens () <$> genWhitespaces <*> genExpr' <*> genWhitespaces

genDeref :: MonadGen m => m (Expr '[] ())
genDeref =
  Gen.subtermM
    genExpr
    (\a ->
        Deref () <$>
        pure a <*>
        genWhitespaces <*>
        genWhitespaces <*>
        genIdent)

-- | This is necessary to prevent generating exponentials that will take forever to evaluate
-- when python does constant folding
genExpr :: MonadGen m => m (Expr '[] ())
genExpr = genExpr' False

genExpr' :: MonadGen m => Bool -> m (Expr '[] ())
genExpr' isExp = Gen.sized $ \n ->
  if n <= 1
  then
    Gen.choice
    [ Ident () <$> genIdent
    , if isExp then genSmallInt else genInt
    , genBool
    , String () <$> genString
    ]
  else
    Gen.resize (n-1) .
    Gen.choice $
      [ genList genExpr
      , genDeref
      , genParens genExpr
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' genExpr
          b <- Gen.resize (n - n') genArgs
          Call () a <$> genWhitespaces <*> pure b
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          op <- genOp
          Gen.subtermM2
            (Gen.resize n' genExpr)
            (Gen.resize (n - n') (genExpr' $ case op of; Exp{} -> True; _ -> False))
            (\a b ->
               BinOp () a <$>
               fmap NonEmpty.toList genWhitespaces1 <*>
               pure op <*>
               fmap NonEmpty.toList genWhitespaces1 <*>
               pure b)
      ]

genAssignable :: MonadGen m => m (Expr '[] ())
genAssignable =
  Gen.scale (max 0 . subtract 1) $
  Gen.choice
    [ genList genAssignable
    , genParens genAssignable
    , genDeref
    ]

genStatement
  :: (HasCallStack, MonadGen m, MonadState [String] m)
  => SyntaxContext
  -> m (Statement '[] ())
genStatement ctxt = Gen.sized $ \n ->
  if n <= 1
  then Gen.element $ [Pass ()] ++ [Break () | _inLoop ctxt]
  else do
    nonlocals <- get
    Gen.resize (n-1) .
      Gen.choice $
        [ Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 1 (n-1))
            a <- Gen.resize n' genParams
            let paramIdents = a ^.. folded.paramName.identValue
            b <-
              Gen.resize
                (n - n')
                (genBlock $
                ctxt
                { _inLoop = False
                , _inFunction =
                    fmap
                      (\b -> union b paramIdents)
                      (_inFunction ctxt) <|>
                    Just paramIdents
                })
            Fundef () <$> genWhitespaces1 <*> genIdent <*> genWhitespaces <*> pure a <*>
              genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
        , Expr () <$> genExpr
        , pure $ Pass ()
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 1 (n-1))
            a <- Gen.resize n' genAssignable
            modify ((a ^.. cosmos._Ident._2.identValue) ++)
            b <- Gen.resize (n - n') genExpr
            Assign () a <$> genWhitespaces <*> genWhitespaces <*> pure b
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 1 (n-1))
            n'' <- Gen.integral (Range.constant 0 (n-n'))
            a <- Gen.resize n' genExpr
            b <- Gen.resize (n - n') (genBlock ctxt)
            c <-
              if n - n' - n'' == 0
              then pure Nothing
              else
                fmap Just $
                (,,,) <$>
                genWhitespaces <*>
                genWhitespaces <*>
                genNewline <*>
                Gen.resize (n - n' - n'') (genBlock ctxt)
            If () <$> fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
              genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b <*> pure c
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 1 (n-1))
            a <- Gen.resize n' genExpr
            b <- Gen.resize (n - n') (genBlock $ ctxt { _inLoop = True})
            While () <$> fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
              genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 2 (n-1))
            Global () <$>
              genWhitespaces1 <*>
              Gen.resize n' (genSizedCommaSep1 genIdent)
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 2 (n-1))
            Del () <$>
              genWhitespaces1 <*>
              Gen.resize n' (genSizedCommaSep1 genIdent)
        ] ++
        [pure (Break ()) | _inLoop ctxt] ++
        [ Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 2 (n-1))
            nonlocals <- get
            Nonlocal () <$>
              genWhitespaces1 <*>
              Gen.resize n' (genSizedCommaSep1 . Gen.element $ MkIdent () <$> nonlocals)
        | isJust (_inFunction ctxt) && not (null nonlocals)
        ] ++
        [ Return () <$>
          fmap NonEmpty.toList genWhitespaces1 <*>
          genExpr
        | isJust (_inFunction ctxt)
        ]
