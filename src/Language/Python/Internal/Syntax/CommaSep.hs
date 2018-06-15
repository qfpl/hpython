{-# language LambdaCase #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Language.Python.Internal.Syntax.CommaSep where

import Control.Lens.Getter ((^.), getting)
import Control.Lens.Lens (lens)
import Control.Lens.Setter ((.~))
import Data.Function ((&))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))

import Language.Python.Internal.Syntax.Token
import Language.Python.Internal.Syntax.Whitespace

data CommaSep a
  = CommaSepNone
  | CommaSepOne a
  | CommaSepMany a [Whitespace] (CommaSep a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

listToCommaSep :: [a] -> CommaSep a
listToCommaSep [] = CommaSepNone
listToCommaSep [a] = CommaSepOne a
  
listToCommaSep (a:as) = CommaSepMany a [Space] $ listToCommaSep as

appendCommaSep :: CommaSep a -> CommaSep a -> CommaSep a
appendCommaSep CommaSepNone b = b
appendCommaSep (CommaSepOne a) CommaSepNone = CommaSepOne a
appendCommaSep (CommaSepOne a) (CommaSepOne b) = CommaSepMany a [] (CommaSepOne b)
appendCommaSep (CommaSepOne a) (CommaSepMany b ws1 cs) = CommaSepMany a [] (CommaSepMany b ws1 cs)
appendCommaSep (CommaSepMany a ws1 cs) b = CommaSepMany a ws1 (appendCommaSep cs b)

-- | Non-empty 'CommaSep'
data CommaSep1 a
  = CommaSepOne1 a
  | CommaSepMany1 a [Whitespace] (CommaSep1 a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

commaSep1Head :: CommaSep1 a -> a
commaSep1Head (CommaSepOne1 a) = a
commaSep1Head (CommaSepMany1 a _ _) = a

instance Semigroup (CommaSep1 a) where
  a <> b =
    CommaSepMany1
      (case a of; CommaSepOne1 x -> x;  CommaSepMany1 x _ _  -> x)
      (case a of; CommaSepOne1 _ -> []; CommaSepMany1 _ ws _ -> ws)
      (case a of; CommaSepOne1 _ -> b;  CommaSepMany1 _ _ x  -> x <> b)

instance Token s t => Token (CommaSep1 s) (CommaSep1 t) where
  unvalidate = fmap unvalidate
  whitespaceAfter =
    lens
      (\case
         CommaSepOne1 a -> a ^. getting whitespaceAfter
         CommaSepMany1 _ _ a -> a ^. getting whitespaceAfter)
      (\cs ws ->
         case cs of
           CommaSepOne1 a ->
             CommaSepOne1 (a & whitespaceAfter .~ ws)
           CommaSepMany1 a b c -> CommaSepMany1 (unvalidate a) b (c & whitespaceAfter .~ ws))

  startChar (CommaSepOne1 a) = startChar a
  startChar (CommaSepMany1 a _ _) = startChar a

  endChar (CommaSepOne1 a) = endChar a
  endChar (CommaSepMany1 _ _ a) = endChar a

listToCommaSep1 :: NonEmpty a -> CommaSep1 a
listToCommaSep1 (a :| as) = go (a:as)
  where
    go [] = error "impossible"
    go [x] = CommaSepOne1 x
    go (x:xs) = CommaSepMany1 x [Space] $ go xs

-- | Non-empty 'CommaSep', optionally terminated by a comma
-- Assumes that the contents consumes trailing whitespace
data CommaSep1' a
  = CommaSepOne1' a (Maybe [Whitespace])
  | CommaSepMany1' a [Whitespace] (CommaSep1' a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

listToCommaSep1' :: [a] -> Maybe (CommaSep1' a)
listToCommaSep1' [] = Nothing
listToCommaSep1' [a] = Just (CommaSepOne1' a Nothing)
listToCommaSep1' (a:as) =
  CommaSepMany1' a [Space] <$> listToCommaSep1' as

instance Token s t => Token (CommaSep1' s) (CommaSep1' t) where
  unvalidate = fmap unvalidate
  whitespaceAfter =
    lens
      (\case
         CommaSepOne1' a b -> fromMaybe (a ^. getting whitespaceAfter) b
         CommaSepMany1' _ _ a -> a ^. getting whitespaceAfter)
      (\cs ws ->
         case cs of
           CommaSepOne1' a b ->
             CommaSepOne1'
               (fromMaybe (a & whitespaceAfter .~ ws) $ b $> unvalidate a)
               (b $> ws)
           CommaSepMany1' a b c ->
             CommaSepMany1' (unvalidate a) b (c & whitespaceAfter .~ ws))

  startChar (CommaSepOne1' a _) = startChar a
  startChar (CommaSepMany1' a _ _) = startChar a

  endChar (CommaSepOne1' a b) = maybe (endChar a) (const ',') b
  endChar (CommaSepMany1' _ _ a) = endChar a
