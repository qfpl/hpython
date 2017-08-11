module Test.Language.Python.AST.Gen where

import Papa

import Data.Functor.Compose
import Data.Separated.After (After(..))
import Data.Separated.Before (Before(..))
import Data.Separated.Between (Between(..), Between'(..))
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Python.AST as AST
import qualified Language.Python.AST.Keywords as AST
import qualified Language.Python.AST.Symbols as AST

genBefore :: Monad m => Gen m s -> Gen m a -> Gen m (Before s a)
genBefore = liftA2 Before

genBeforeF
  :: Monad m
  => Gen m s
  -> Gen m (f a)
  -> Gen m (Compose (Before s) f a)
genBeforeF ms = fmap Compose . genBefore ms

genAfter :: Monad m => Gen m s -> Gen m a -> Gen m (After s a)
genAfter = liftA2 After

genBetween :: Monad m => Gen m s -> Gen m t -> Gen m a -> Gen m (Between s t a)
genBetween ms mt ma = Between <$> ms <*> ma <*> mt

genBetween' :: Monad m => Gen m s -> Gen m a -> Gen m (Between' s a)
genBetween' ms ma = Between' <$> genBetween ms ms ma

genNewlineChar :: Monad m => Gen m AST.NewlineChar
genNewlineChar = Gen.element [AST.CR, AST.LF, AST.CRLF]

genWhitespaceChar :: Monad m => Gen m AST.WhitespaceChar
genWhitespaceChar =
  Gen.choice
    [ pure AST.Space
    , pure AST.Tab
    , AST.Continued <$> genNewlineChar
    ]

genListF :: Monad m => Gen m (f a) -> Gen m (Compose [] f a)
genListF ma =
  Compose <$>
  Gen.list (Range.linear 0 10) ma
  
genMaybeF :: Monad m => Gen m (f a) -> Gen m (Compose Maybe f a)
genMaybeF ma = Compose <$> Gen.maybe ma
    
genWhitespace1 :: Monad m => Gen m (NonEmpty AST.WhitespaceChar)
genWhitespace1 = Gen.nonEmpty (Range.linear 1 10) genWhitespaceChar

genWhitespaceAfter
  :: Monad m
  => Gen m a
  -> Gen m (After [AST.WhitespaceChar] a)
genWhitespaceAfter ma = After <$> genWhitespace <*> ma

genWhitespaceBefore
  :: Monad m
  => Gen m a
  -> Gen m (Before [AST.WhitespaceChar] a)
genWhitespaceBefore ma = Before <$> genWhitespace <*> ma

genWhitespaceBeforeF
  :: Monad m
  => Gen m (f a)
  -> Gen m (Compose (Before [AST.WhitespaceChar]) f a)
genWhitespaceBeforeF = fmap Compose . genWhitespaceBefore

genWhitespaceAfter1
  :: Monad m
  => Gen m a
  -> Gen m (After (NonEmpty AST.WhitespaceChar) a)
genWhitespaceAfter1 ma = After <$> genWhitespace1 <*> ma

genWhitespace :: Monad m => Gen m [AST.WhitespaceChar]
genWhitespace = Gen.list (Range.linear 0 10) genWhitespaceChar

genBetweenWhitespace
  :: Monad m
  => Gen m a
  -> Gen m (Between' [AST.WhitespaceChar] a)
genBetweenWhitespace = genBetween' genWhitespace

genBetweenWhitespaceF
  :: Monad m
  => Gen m (f a)
  -> Gen m (Compose (Between' [AST.WhitespaceChar]) f a)
genBetweenWhitespaceF = fmap Compose . genBetweenWhitespace
    
genIfThenElse :: Monad m => Gen m (AST.IfThenElse ())
genIfThenElse = _

genTermOp :: Monad m => Gen m AST.TermOp
genTermOp =
  Gen.element
    [ AST.TermMult
    , AST.TermAt
    , AST.TermFloorDiv
    , AST.TermDiv
    , AST.TermMod
    ]
    
genAtom :: Monad m => Gen m (AST.Atom ())
genAtom = _

genArgList :: Monad m => Gen m (AST.ArgList ())
genArgList = _

genSubscriptList :: Monad m => Gen m (AST.SubscriptList ())
genSubscriptList = _

genIdentifier :: Monad m => Gen m (AST.Identifier ())
genIdentifier = Gen.frequency [(_, Gen.upper), (_, Gen.lower), (_, pure '_')]

genTrailer :: Monad m => Gen m (AST.Trailer ())
genTrailer =
  Gen.recursive Gen.choice
    [ AST.TrailerAccess <$>
      genWhitespaceBeforeF genIdentifier <*>
      pure ()
    ]
    [ AST.TrailerCall <$>
      genBetweenWhitespaceF (genMaybeF genArgList) <*>
      pure ()
    , AST.TrailerSubscript <$>
      genBetweenWhitespaceF (genMaybeF genSubscriptList) <*>
      pure ()
    ]
    
genAtomExpr :: Monad m => Gen m (AST.AtomExpr ())
genAtomExpr =
  Gen.recursive Gen.choice
    []
    [ AST.AtomExpr <$>
      genMaybeF (genWhitespaceAfter1 $ pure AST.KAwait) <*>
      genAtom <*>
      genListF (genWhitespaceBeforeF genTrailer) <*>
      pure ()
    ]
    
genPower :: Monad m => Gen m (AST.Power ())
genPower =
  Gen.recursive Gen.choice
    []
    [ AST.Power <$>
      genAtomExpr <*>
      genMaybeF
        (genBeforeF
          (genWhitespaceAfter $ pure AST.DoubleAsterisk)
          genFactor) <*>
      pure ()
    ]

genFactorOp :: Monad m => Gen m AST.FactorOp
genFactorOp =
  Gen.element
    [ AST.FactorNeg
    , AST.FactorPos
    , AST.FactorInv
    ]
    
genFactor :: Monad m => Gen m (AST.Factor ())
genFactor =
  Gen.recursive Gen.choice
    []
    [ AST.FactorNone <$>
      genPower <*>
      pure ()
    , AST.FactorSome <$>
      genBeforeF (genWhitespaceAfter genFactorOp) genFactor <*>
      pure ()
    ]

genTerm :: Monad m => Gen m (AST.Term ())
genTerm =
  Gen.recursive Gen.choice
    []
    [ AST.Term <$>
      genFactor <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace genTermOp)
          genFactor) <*>
      pure ()
    ]

genArithExpr :: Monad m => Gen m (AST.ArithExpr ())
genArithExpr =
  Gen.recursive Gen.choice
    []
    [ AST.ArithExpr <$>
      genTerm <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $
             Gen.element [Left AST.Plus, Right AST.Minus])
          genTerm) <*>
      pure ()
    ]

genShiftExpr :: Monad m => Gen m (AST.ShiftExpr ())
genShiftExpr =
  Gen.recursive Gen.choice
    []
    [ AST.ShiftExpr <$>
      genArithExpr <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $
             Gen.element [Left AST.DoubleLT, Right AST.DoubleGT])
          genArithExpr) <*>
      pure ()
    ]

genAndExpr :: Monad m => Gen m (AST.AndExpr ())
genAndExpr =
  Gen.recursive Gen.choice
    []
    [ AST.AndExpr <$>
      genShiftExpr <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Ampersand)
          genShiftExpr) <*>
      pure ()
    ]

genXorExpr :: Monad m => Gen m (AST.XorExpr ())
genXorExpr =
  Gen.recursive Gen.choice
    []
    [ AST.XorExpr <$>
      genAndExpr <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Caret)
          genAndExpr) <*>
      pure ()
    ]

genExpr :: Monad m => Gen m (AST.Expr ())
genExpr =
  Gen.recursive Gen.choice
    []
    [ AST.Expr <$>
      genXorExpr <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Pipe)
          genXorExpr) <*>
      pure ()
    ]

genCompOperator :: Monad m => Gen m AST.CompOperator
genCompOperator =
  Gen.choice
    [ pure AST.CompLT
    , pure AST.CompGT
    , pure AST.CompEq
    , pure AST.CompGEq
    , pure AST.CompLEq
    , pure AST.CompNEq
    , AST.CompIs <$> genWhitespaceChar
    , AST.CompIsNot <$> genWhitespace1 <*> genWhitespaceChar
    , AST.CompIn <$> genWhitespaceChar
    , AST.CompNotIn <$> genWhitespace1 <*> genWhitespaceChar
    ]

genComparison :: Monad m => Gen m (AST.Comparison ())
genComparison =
  Gen.recursive Gen.choice
    []
    [ AST.Comparison <$>
      genExpr <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace genCompOperator)
          genExpr) <*>
      pure ()
    ]

genNotTest :: Monad m => Gen m (AST.NotTest ())
genNotTest =
  Gen.recursive Gen.choice
    []
    [ AST.NotTestSome <$>
      genBeforeF
        (genWhitespaceAfter1 $ pure AST.KNot)
        genNotTest <*>
      pure ()
    , AST.NotTestNone <$> genComparison <*> pure ()
    ]
    
genAndTest :: Monad m => Gen m (AST.AndTest ())
genAndTest =
  Gen.recursive Gen.choice
    []
    [ AST.AndTest <$>
      genNotTest <*>
      (genListF
        (genBeforeF
          (genBetween' genWhitespace1 $ pure AST.KAnd)
          genAndTest)) <*>
      pure ()
    ]

genOrTest :: Monad m => Gen m (AST.OrTest ())
genOrTest =
  Gen.recursive Gen.choice
    []
    [ AST.OrTest <$>
      genAndTest <*>
      (genListF
        (genBeforeF
          (genBetween' genWhitespace1 $ pure AST.KOr)
          genAndTest)) <*>
      pure ()
    ]

genTest :: Monad m => Gen m (AST.Test ())
genTest =
  Gen.recursive Gen.choice
    []
    [ AST.TestCond <$>
      genOrTest <*>
      (Compose <$>
         Gen.maybe
           (Compose <$>
              genBefore
                genWhitespace1 
                genIfThenElse)) <*>
      pure ()
    , pure AST.TestLambdef
    ]
