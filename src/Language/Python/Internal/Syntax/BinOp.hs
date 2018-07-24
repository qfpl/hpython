{-# language LambdaCase #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Syntax.BinOp where

import Control.Lens.Getter ((^.))
import Control.Lens.Lens (lens)
import Control.Lens.TH (makeLenses)
import Data.Functor (($>))
import Data.Semigroup ((<>))

import Language.Python.Internal.Syntax.Whitespace

data BinOp a
  = Is a [Whitespace]
  | IsNot a [Whitespace] [Whitespace]
  | In a [Whitespace]
  | NotIn a [Whitespace] [Whitespace]
  | Minus a [Whitespace]
  | Exp a [Whitespace]
  | BoolAnd a [Whitespace]
  | BoolOr a [Whitespace]
  | Equals a [Whitespace]
  | Lt a [Whitespace]
  | LtEquals a [Whitespace]
  | Gt a [Whitespace]
  | GtEquals a [Whitespace]
  | NotEquals a [Whitespace]
  | Multiply a [Whitespace]
  | Divide a [Whitespace]
  | FloorDivide a [Whitespace]
  | Percent a [Whitespace]
  | Plus a [Whitespace]
  | BitOr a [Whitespace]
  | BitXor a [Whitespace]
  | BitAnd a [Whitespace]
  | ShiftLeft a [Whitespace]
  | ShiftRight a [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable)

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
         Equals _ a -> a
         Lt _ a -> a
         LtEquals _ a -> a
         Gt _ a -> a
         GtEquals _ a -> a
         NotEquals _ a -> a
         BitOr _ a -> a
         BitXor _ a -> a
         BitAnd _ a -> a
         ShiftLeft _ a -> a
         ShiftRight _ a -> a
         Percent _ a -> a)
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
           Equals a _ -> Equals a ws
           Lt a _ -> Lt a ws
           LtEquals a _ -> LtEquals a ws
           Gt a _ -> Gt a ws
           GtEquals a _ -> GtEquals a ws
           NotEquals a _ -> NotEquals a ws
           BitOr a _ -> BitOr a ws
           BitAnd a _ -> BitAnd a ws
           BitXor a _ -> BitXor a ws
           ShiftLeft a _ -> ShiftLeft a ws
           ShiftRight a _ -> ShiftRight a ws
           Percent a _ -> Equals a ws)

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
  , entry1 IsNot 10 L
  , entry In 10 L
  , entry1 NotIn 10 L
  , entry Equals 10 L
  , entry Lt 10 L
  , entry LtEquals 10 L
  , entry Gt 10 L
  , entry GtEquals 10 L
  , entry NotEquals 10 L
  , entry BitOr 14 L
  , entry BitXor 15 L
  , entry BitAnd 16 L
  , entry ShiftLeft 17 L
  , entry ShiftRight 17 L
  , entry Minus 20 L
  , entry Plus 20 L
  , entry Multiply 25 L
  , entry Divide 25 L
  , entry FloorDivide 25 L
  , entry Percent 25 L
  , entry Exp 30 R
  ]
  where
    entry a = OpEntry (a () [])
    entry1 a = OpEntry (a () [] [])

sameOperator :: BinOp a -> BinOp a' -> Bool
sameOperator op op' =
  case (op, op') of
    (BoolOr{}, BoolOr{}) -> True
    (BoolAnd{}, BoolAnd{}) -> True
    (Is{}, Is{}) -> True
    (IsNot{}, IsNot{}) -> True
    (In{}, In{}) -> True
    (NotIn{}, NotIn{}) -> True
    (Equals{}, Equals{}) -> True
    (Lt{}, Lt{}) -> True
    (LtEquals{}, LtEquals{}) -> True
    (Gt{}, Gt{}) -> True
    (GtEquals{}, GtEquals{}) -> True
    (NotEquals{}, NotEquals{}) -> True
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
    _ -> False

lookupOpEntry :: BinOp a -> [OpEntry] -> OpEntry
lookupOpEntry op =
  go (op $> ())
  where
    go op [] = error $ show op <> " not found in operator table"
    go op (x:xs)
      | sameOperator (x ^. opOperator) op = x
      | otherwise = go op xs
