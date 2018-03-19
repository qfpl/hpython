{-# language DataKinds #-}
module Generators where

import Control.Applicative
import Control.Lens (sumOf, folded, _3, to)
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax

genNewline :: MonadGen m => m Newline
genNewline = Gen.element [LF, CR, CRLF]

whitespaceSize :: Whitespace -> Size
whitespaceSize Space = 1
whitespaceSize Tab = 1
whitespaceSize (Continued _ ws) = 1 + sum (fmap whitespaceSize ws)

genSizedWhitespace :: MonadGen m => m [Whitespace]
genSizedWhitespace = Gen.sized $ \n ->
  if n == 0
  then pure []
  else if n == 1
  then Gen.element [[Space], [Tab]]
  else do
    Gen.resize (n-1) $ do
      w <-
        Gen.choice
          [ pure Space
          , pure Tab
          , do
              n' <- Gen.integral (Range.constant 1 (n-1))
              Gen.resize n' $ Continued <$> genNewline <*> genSizedWhitespace
          ]
      l <- Gen.resize (n - whitespaceSize w) genSizedWhitespace
      pure $ w : l

genWhitespaces :: MonadGen m => m [Whitespace]
genWhitespaces = do
  n <- Gen.integral (Range.constant 0 10)
  Gen.resize n genSizedWhitespace

genWhitespaces1 :: MonadGen m => m (NonEmpty Whitespace)
genWhitespaces1 = do
  n <- Gen.integral (Range.constant 0 9)
  liftA2 (:|) (head <$> Gen.resize 1 genSizedWhitespace) (Gen.resize n genSizedWhitespace)

genString :: MonadGen m => m String
genString = Gen.list (Range.constant 0 50) (Gen.filter (/='\0') Gen.latin1)

genIdent :: MonadGen m => m (Ident '[] ())
genIdent =
  MkIdent () <$>
  liftA2 (:)
    (Gen.choice [Gen.alpha, pure '_'])
    (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_']))

genCommaSep :: MonadGen m => Range Int -> m a -> m (CommaSep a)
genCommaSep r m = do
  s <- Gen.integral r
  Gen.sized $ \n -> go (n `div` Size s) s
  where
    go s 0 = pure CommaSepNone
    go s 1 =
      Gen.choice
        [ CommaSepOne <$> Gen.resize s m
        , CommaSepMany <$>
          Gen.resize s m <*>
          genWhitespaces <*>
          genWhitespaces <*>
          go s 0
        ]
    go s n =
      CommaSepMany <$>
      Gen.resize s m <*>
      genWhitespaces <*>
      genWhitespaces <*>
      go s (n-1)

genArg :: MonadGen m => m (Arg '[] ())
genArg =
  Gen.recursive Gen.choice
    [ PositionalArg () <$> genExpr ]
    [ KeywordArg () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genExpr ]

genParam :: MonadGen m => m (Param '[] ())
genParam =
  Gen.choice
    [ PositionalParam () <$> genIdent
    , KeywordParam () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genExpr
    ]

blockSize :: Block v a -> Size
blockSize (Block b) = sumOf (folded._3.to statementSize) b

argSize :: Arg v a -> Size
argSize (PositionalArg _ a) = 1 + exprSize a
argSize (KeywordArg _ _ _ _ a) = 1 + exprSize a

paramSize :: Param v a -> Size
paramSize (PositionalParam _ _) = 1
paramSize (KeywordParam _ _ _ _ a) = 1 + exprSize a

commaSepSize :: (a -> Size) -> CommaSep a -> Size
commaSepSize _ CommaSepNone = 1
commaSepSize f (CommaSepOne a) = 1 + f a
commaSepSize f (CommaSepMany a _ _ c) = 1 + f a + commaSepSize f c

exprSize :: Expr v a -> Size
exprSize (List _ _ a _) = 1 + commaSepSize exprSize a
exprSize (Deref _ a _ _ _) = 1 + exprSize a
exprSize (Call _ a _ b) = 1 + exprSize a + commaSepSize argSize b
exprSize (None _) = 1
exprSize (BinOp _ a _ _ _ b) = 1 + exprSize a + exprSize b
exprSize (Negate _ _ a) = 1 + exprSize a
exprSize (Parens _ _ a _) = 1 + exprSize a
exprSize Ident{} = 1
exprSize Int{} = 1
exprSize Bool{} = 1
exprSize String{} = 1

genSizedArg :: MonadGen m => m (Arg '[] ())
genSizedArg = Gen.sized $ \n ->
  if n <= 1
  then -- error "arg for size 1"
    Gen.choice
      [ PositionalArg () <$> genSizedExpr
      , KeywordArg () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genSizedExpr
      ]
  else
    Gen.resize (n-1) $
    Gen.choice
      [ PositionalArg () <$> genSizedExpr
      , KeywordArg () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genSizedExpr
      ]

genSizedParam :: MonadGen m => m (Param '[] ())
genSizedParam = Gen.sized $ \n ->
  if n <= 1
  then PositionalParam () <$> genIdent
  else
    Gen.resize (n-1) $
    KeywordParam () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genSizedExpr

genSizedBlock :: MonadGen m => m (Block '[] ())
genSizedBlock = do
  indent <- NonEmpty.toList <$> genWhitespaces1
  go indent
  where
    go indent =
      Gen.sized $ \n ->
      if n <= 1
        then do
          s1 <- genSizedStatement
          Block <$>
            liftA2 (:|)
              ((,,,) () indent <$> pure s1 <*> Gen.maybe genNewline)
              (pure [])
        else do
          n' <- Gen.integral (Range.constant 1 (n-1))
          s1 <- Gen.resize n' genSizedStatement
          let n'' = n - n'
          b <- Gen.resize n'' (go indent)
          Block <$>
            liftA2 NonEmpty.cons
              ((,,,) () indent <$> pure s1 <*> Gen.maybe genNewline)
              (pure $ unBlock b)

genSizedCommaSep :: MonadGen m => (a -> Size) -> m a -> m (CommaSep a)
genSizedCommaSep sm ma = Gen.sized $ \n ->
  if n <= 1
  then pure CommaSepNone
  else
    Gen.resize (n-1) $
    Gen.choice
      [ CommaSepOne <$> ma
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' ma
          Gen.subtermM
            (Gen.resize (n - n') $ genSizedCommaSep sm ma)
            (\b -> CommaSepMany a <$> genWhitespaces <*> genWhitespaces <*> pure b)
      ]

genSizedExpr :: MonadGen m => m (Expr '[] ())
genSizedExpr = Gen.sized $ \n ->
  if n <= 1
  then Gen.choice [Ident () <$> genIdent, genInt, genBool, String () <$> genString]
  else
    Gen.resize (n-1) $
    Gen.choice $
      [ List () <$>
        genWhitespaces <*>
        genSizedCommaSep exprSize genSizedExpr <*>
        genWhitespaces
      , Gen.subtermM
          genSizedExpr
          (\a ->
             Deref () <$>
             pure a <*>
             genWhitespaces <*>
             genWhitespaces <*>
             genIdent)
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' genSizedExpr
          b <- Gen.resize (n - n') $ genSizedCommaSep argSize genSizedArg
          Call () a <$> genWhitespaces <*> pure b
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          Gen.subtermM2
            (Gen.resize n' genSizedExpr)
            (Gen.resize (n - n') genSizedExpr)
            (\a b -> BinOp () a <$> genWhitespaces <*> genOp <*> genWhitespaces <*> pure b)
      , Parens () <$> genWhitespaces <*> genSizedExpr <*> genWhitespaces
      ]

statementSize :: Statement v a -> Size
statementSize (Fundef _ _ _ _ a _ _ _ b) = 1 + commaSepSize paramSize a + blockSize b
statementSize (Return _ _ a) = 1 + exprSize a
statementSize (Expr _ a) = 1 + exprSize a
statementSize (If _ _ a _ _ _ b c) =
  1 + exprSize a + blockSize b + maybe 0 (\(_, _, _, b) -> blockSize b) c
statementSize (While _ _ a _ _ _ b) = 1 + exprSize a + blockSize b
statementSize (Assign _ a _ _ b) = 1 + exprSize a + exprSize b
statementSize Pass{} = 1
statementSize Break{} = 1
statementSize (Global _ _ cs) = Size $ 1 + length cs
statementSize (Nonlocal _ _ cs) = Size $ 1 + length cs
statementSize (Del _ _ cs) = Size $ 1 + length cs

genSizedStatement :: MonadGen m => m (Statement '[] ())
genSizedStatement = Gen.sized $ \n ->
  if n <= 1
  then Gen.element [Pass (), Break ()]
  else
    Gen.resize (n-1) $
    Gen.choice
      [ Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' $ genSizedCommaSep paramSize genSizedParam
          b <- Gen.resize (n - n') genSizedBlock
          Fundef () <$> genWhitespaces1 <*> genIdent <*> genWhitespaces <*> pure a <*>
            genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
      ]

genList :: MonadGen m => m (Expr '[] ())
genList =
  List () <$>
  genWhitespaces <*>
  genCommaSep (Range.linear 0 5) genExpr <*>
  genWhitespaces

genDeref :: MonadGen m => m (Expr '[] ())
genDeref =
  Deref () <$>
  genExpr <*>
  genWhitespaces <*>
  genWhitespaces <*>
  genIdent

genCall :: MonadGen m => m (Expr '[] ())
genCall =
  Call () <$>
  genExpr <*>
  genWhitespaces <*>
  genCommaSep (Range.linear 0 5) genArg

genNone :: MonadGen m => m (Expr '[] ())
genNone = pure $ None ()

genBool :: MonadGen m => m (Expr '[] ())
genBool = Bool () <$> Gen.bool

genBinOp :: MonadGen m => m (Expr '[] ())
genBinOp =
  BinOp () <$>
  genExpr <*>
  genWhitespaces <*>
  genOp <*>
  genWhitespaces <*>
  genExpr

genOp :: MonadGen m => m (BinOp ())
genOp = Gen.element $ _opOperator <$> operatorTable

genNegate :: MonadGen m => m (Expr '[] ())
genNegate = Negate () <$> genWhitespaces <*> genExpr

genParens :: MonadGen m => m (Expr '[] ())
genParens = Parens () <$> genWhitespaces <*> genExpr <*> genWhitespaces

genInt :: MonadGen m => m (Expr '[] ())
genInt = Int () <$> Gen.integral (Range.constant (-2^16) (2^16))

genExpr :: MonadGen m => m (Expr '[] ())
genExpr =
  Gen.recursive Gen.choice
    [ genNone
    , Ident () <$> genIdent
    , genInt
    , genBool
    , String () <$> genString
    ]
    [ genList
    , genDeref
    , genCall
    , genBinOp
    , genNegate
    , genParens
    ]

genBlock :: MonadGen m => Range Int -> m (Block '[] ())
genBlock r = do
  n <- Size <$> Gen.integral r
  when (n == 0) $ error "cannot generate block of size 0"
  Block . NonEmpty.fromList <$> Gen.sized (\s -> go (s `div` n) n)
  where
    go _ 0 = pure []
    go s n =
      liftA2
        (:)
        (Gen.resize s $
         (,,,) () <$>
         genWhitespaces <*>
         genStatement <*>
         Gen.maybe genNewline)
        (go s $ n-1)

genStatement :: MonadGen m => m (Statement '[] ())
genStatement =
  Gen.recursive Gen.choice
    [ genPass
    , genBreak
    ]
    [ genAssign
    , genEx
    , genReturn
    , genFundef
    , genIf
    , genWhile
    ]
  where
    genFundef =
      Fundef () <$>
      genWhitespaces1 <*>
      genIdent <*>
      genWhitespaces <*>
      genCommaSep (Range.linear 0 5) genParam <*>
      genWhitespaces <*>
      genWhitespaces <*>
      genNewline <*>
      genBlock (Range.exponential 1 10)
    genReturn = Return () <$> genWhitespaces <*> genExpr
    genEx = Expr () <$> genExpr
    genIf =
      If () <$>
      genWhitespaces <*>
      genExpr <*>
      genWhitespaces <*>
      genWhitespaces <*>
      genNewline <*>
      genBlock (Range.exponential 1 10) <*>
      Gen.maybe
        ((,,,) <$>
         genWhitespaces <*>
         genWhitespaces <*>
         genNewline <*>
         genBlock (Range.exponential 1 10))
    genWhile =
      While () <$>
      genWhitespaces <*>
      genExpr <*>
      genWhitespaces <*>
      genWhitespaces <*>
      genNewline <*>
      genBlock (Range.exponential 1 10)
    genPass = pure $ Pass ()
    genBreak = pure $ Break ()
    genAssign =
      Assign () <$>
      genExpr <*>
      genWhitespaces <*>
      genWhitespaces <*>
      genExpr
