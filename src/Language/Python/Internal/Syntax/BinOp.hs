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
  | Percent a [Whitespace]
  | Plus a [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (BinOp a) where
  trailingWhitespace =
    lens
      (\case
         Is _ a -> a
         Minus _ a -> a
         Exp _ a -> a
         BoolAnd _ a -> a
         BoolOr _ a -> a
         Multiply _ a -> a
         Divide _ a -> a
         Plus _ a -> a
         Equals _ a -> a
         Lt _ a -> a
         LtEquals _ a -> a
         Gt _ a -> a
         GtEquals _ a -> a
         NotEquals _ a -> a
         Percent _ a -> a)
      (\op ws ->
         case op of
           Is a _ -> Is a ws
           Minus a _ -> Minus a ws
           Exp a _ -> Exp a ws
           BoolAnd a _ -> BoolAnd a ws
           BoolOr a _ -> BoolOr a ws
           Multiply a _ -> Multiply a ws
           Divide a _ -> Divide a ws
           Plus a _ -> Plus a ws
           Equals a _ -> Equals a ws
           Lt a _ -> Lt a ws
           LtEquals a _ -> LtEquals a ws
           Gt a _ -> Gt a ws
           GtEquals a _ -> GtEquals a ws
           NotEquals a _ -> NotEquals a ws
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
  , entry Equals 10 L
  , entry Lt 10 L
  , entry LtEquals 10 L
  , entry Gt 10 L
  , entry GtEquals 10 L
  , entry NotEquals 10 L
  , entry Minus 20 L
  , entry Plus 20 L
  , entry Multiply 25 L
  , entry Divide 25 L
  , entry Percent 25 L
  , entry Exp 30 R
  ]
  where
    entry a = OpEntry (a () [])

sameOperator :: BinOp a -> BinOp a' -> Bool
sameOperator op op' =
  case (op, op') of
    (BoolOr{}, BoolOr{}) -> True
    (BoolAnd{}, BoolAnd{}) -> True
    (Is{}, Is{}) -> True
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
    (Exp{}, Exp{}) -> True
    (Percent{}, Percent{}) -> True
    _ -> False

lookupOpEntry :: BinOp a -> [OpEntry] -> OpEntry
lookupOpEntry op =
  go (op $> ())
  where
    go op [] = error $ show op <> " not found in operator table"
    go op (x:xs)
      | sameOperator (x ^. opOperator) op = x
      | otherwise = go op xs
