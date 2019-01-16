{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Syntax.Operator.Binary
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

This module contains a datatype for binary operators and a precedence table
with associated operations. This presentation of operators is simpler and more
flexible than hard-coding them into the syntax tree.
-}

module Language.Python.Syntax.Operator.Binary where

import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.TH (makeLenses)
import Data.Functor (($>))
import Data.Generics.Product.Typed (typed)
import Data.Semigroup ((<>))
import GHC.Generics (Generic)

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Whitespace

-- | A Python binary operator, such as @+@, along with its trailing 'Whitespace'
data BinOp a
  -- | @a is b@
  = Is (Ann a) [Whitespace]
  -- | @a is not b@
  | IsNot (Ann a) [Whitespace] [Whitespace]
  -- | @a in b@
  | In (Ann a) [Whitespace]
  -- | @a not in b@
  | NotIn (Ann a) [Whitespace] [Whitespace]
  -- | @a - b@
  | Minus (Ann a) [Whitespace]
  -- | @a ** b@
  | Exp (Ann a) [Whitespace]
  -- | @a and b@
  | BoolAnd (Ann a) [Whitespace]
  -- | @a or b@
  | BoolOr (Ann a) [Whitespace]
  -- | @a == b@
  | Eq (Ann a) [Whitespace]
  -- | @a < b@
  | Lt (Ann a) [Whitespace]
  -- | @a <= b@
  | LtEq (Ann a) [Whitespace]
  -- | @a > b@
  | Gt (Ann a) [Whitespace]
  -- | @a >= b@
  | GtEq (Ann a) [Whitespace]
  -- | @a != b@
  | NotEq (Ann a) [Whitespace]
  -- | @a * b@
  | Multiply (Ann a) [Whitespace]
  -- | @a / b@
  | Divide (Ann a) [Whitespace]
  -- | @a // b@
  | FloorDivide (Ann a) [Whitespace]
  -- | @a % b@
  | Percent (Ann a) [Whitespace]
  -- | @a + b@
  | Plus (Ann a) [Whitespace]
  -- | @a | b@
  | BitOr (Ann a) [Whitespace]
  -- | @a ^ b@
  | BitXor (Ann a) [Whitespace]
  -- | @a & b@
  | BitAnd (Ann a) [Whitespace]
  -- | @a << b@
  | ShiftLeft (Ann a) [Whitespace]
  -- | @a >> b@
  | ShiftRight (Ann a) [Whitespace]
  -- | @a @ b@
  | At (Ann a) [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn BinOp where
  annot :: forall a. Lens' (BinOp a) (Ann a)
  annot = typed @(Ann a)

instance HasTrailingWhitespace (BinOp a) where
  trailingWhitespace =
    lens
      (\case
         Is _ a -> a
         IsNot _ _ a -> a
         In _ a -> a
         NotIn _ _ a -> a
         Minus _ a -> a
         Exp _ a -> a
         BoolAnd _ a -> a
         BoolOr _ a -> a
         Multiply _ a -> a
         Divide _ a -> a
         FloorDivide _ a -> a
         Plus _ a -> a
         Eq _ a -> a
         Lt _ a -> a
         LtEq _ a -> a
         Gt _ a -> a
         GtEq _ a -> a
         NotEq _ a -> a
         BitOr _ a -> a
         BitXor _ a -> a
         BitAnd _ a -> a
         ShiftLeft _ a -> a
         ShiftRight _ a -> a
         Percent _ a -> a
         At _ a -> a)
      (\op ws ->
         case op of
           Is a _ -> Is a ws
           IsNot a b _ -> IsNot a b ws
           In a _ -> In a ws
           NotIn a b _ -> NotIn a b ws
           Minus a _ -> Minus a ws
           Exp a _ -> Exp a ws
           BoolAnd a _ -> BoolAnd a ws
           BoolOr a _ -> BoolOr a ws
           Multiply a _ -> Multiply a ws
           Divide a _ -> Divide a ws
           FloorDivide a _ -> FloorDivide a ws
           Plus a _ -> Plus a ws
           Eq a _ -> Eq a ws
           Lt a _ -> Lt a ws
           LtEq a _ -> LtEq a ws
           Gt a _ -> Gt a ws
           GtEq a _ -> GtEq a ws
           NotEq a _ -> NotEq a ws
           BitOr a _ -> BitOr a ws
           BitAnd a _ -> BitAnd a ws
           BitXor a _ -> BitXor a ws
           ShiftLeft a _ -> ShiftLeft a ws
           ShiftRight a _ -> ShiftRight a ws
           Percent a _ -> Eq a ws
           At a _ -> At a ws)

-- | The associativity of an operator. Each operator is either left-associative or right associative.
--
-- Left associative:
--
-- @
-- x + y + z = (x + y) + z
-- @
--
-- Right associative:
--
-- @
-- x + y + z = x + (y + z)
-- @
data Assoc = L | R deriving (Eq, Show)

-- | An operator along with its precedence and associativity.
data OpEntry
  = OpEntry
  { _opOperator :: BinOp ()
  , _opPrec :: Int
  , _opAssoc :: Assoc
  }
makeLenses ''OpEntry

-- | 'operatorTable' is a list of all operators in ascending order of precedence.
operatorTable :: [OpEntry]
operatorTable =
  [ entry BoolOr 4 L
  , entry BoolAnd 5 L
  , entry Is 10 L
  , entry1 IsNot 10 L
  , entry In 10 L
  , entry1 NotIn 10 L
  , entry Eq 10 L
  , entry Lt 10 L
  , entry LtEq 10 L
  , entry Gt 10 L
  , entry GtEq 10 L
  , entry NotEq 10 L
  , entry BitOr 14 L
  , entry BitXor 15 L
  , entry BitAnd 16 L
  , entry ShiftLeft 17 L
  , entry ShiftRight 17 L
  , entry Minus 20 L
  , entry Plus 20 L
  , entry Multiply 25 L
  , entry At 25 L
  , entry Divide 25 L
  , entry FloorDivide 25 L
  , entry Percent 25 L
  , entry Exp 30 R
  ]
  where
    entry a = OpEntry (a (Ann ()) [])
    entry1 a = OpEntry (a (Ann ()) [] [])

-- | Compare two 'BinOp's to determine whether they represent the same operator, ignoring annotations and trailing whitespace.
sameOperator :: BinOp a -> BinOp a' -> Bool
sameOperator op op' =
  case (op, op') of
    (BoolOr{}, BoolOr{}) -> True
    (BoolAnd{}, BoolAnd{}) -> True
    (Is{}, Is{}) -> True
    (IsNot{}, IsNot{}) -> True
    (In{}, In{}) -> True
    (NotIn{}, NotIn{}) -> True
    (Eq{}, Eq{}) -> True
    (Lt{}, Lt{}) -> True
    (LtEq{}, LtEq{}) -> True
    (Gt{}, Gt{}) -> True
    (GtEq{}, GtEq{}) -> True
    (NotEq{}, NotEq{}) -> True
    (Minus{}, Minus{}) -> True
    (Plus{}, Plus{}) -> True
    (Multiply{}, Multiply{}) -> True
    (Divide{}, Divide{}) -> True
    (FloorDivide{}, FloorDivide{}) -> True
    (Exp{}, Exp{}) -> True
    (Percent{}, Percent{}) -> True
    (BitOr{}, BitOr{}) -> True
    (BitXor{}, BitXor{}) -> True
    (BitAnd{}, BitAnd{}) -> True
    (ShiftLeft{}, ShiftLeft{}) -> True
    (ShiftRight{}, ShiftRight{}) -> True
    (At{}, At{}) -> True
    _ -> False

-- | Is a 'BinOp' a comparison, such as @<=@
isComparison :: BinOp a -> Bool
isComparison a =
  case a of
    Is{} -> True
    IsNot{} -> True
    In{} -> True
    NotIn{} -> True
    Eq{} -> True
    Lt{} -> True
    LtEq{} -> True
    Gt{} -> True
    GtEq{} -> True
    NotEq{} -> True
    _ -> False

-- | Retrieve the information for a given operator from the operator table.
lookupOpEntry :: BinOp a -> [OpEntry] -> OpEntry
lookupOpEntry op =
  go (op $> ())
  where
    go o [] = error $ show o <> " not found in operator table"
    go o (x:xs)
      | sameOperator (x ^. opOperator) o = x
      | otherwise = go o xs
