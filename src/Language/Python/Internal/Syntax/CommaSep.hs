{-# language LambdaCase #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{-|
Module      : Language.Python.Internal.Syntax.CommaSep
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Syntax.CommaSep where

import Control.Lens.Getter ((^.))
import Control.Lens.Iso (Iso, iso)
import Control.Lens.Lens (lens)
import Control.Lens.Setter ((.~))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))

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

instance HasTrailingWhitespace s => HasTrailingWhitespace (CommaSep1 s) where
  trailingWhitespace =
    lens
      (\case
         CommaSepOne1 a -> a ^. trailingWhitespace
         CommaSepMany1 _ _ a -> a ^. trailingWhitespace)
      (\cs ws ->
         case cs of
           CommaSepOne1 a ->
             CommaSepOne1 (a & trailingWhitespace .~ ws)
           CommaSepMany1 a b c -> CommaSepMany1 (coerce a) b (c & trailingWhitespace .~ ws))

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

_CommaSep1'
  :: Iso
       (a, [([Whitespace], a)], Maybe [Whitespace])
       (b, [([Whitespace], b)], Maybe [Whitespace])
       (CommaSep1' a)
       (CommaSep1' b)
_CommaSep1' = iso toCs fromCs
  where
    toCs (a, [], b) = CommaSepOne1' a b
    toCs (a, (b, c) : bs, d) = CommaSepMany1' a b $ toCs (c, bs, d)

    fromCs (CommaSepOne1' a b) = (a, [], b)
    fromCs (CommaSepMany1' a b c) =
      let
        (d, e, f) = fromCs c
      in
        (a, (b, d) : e, f)

listToCommaSep1' :: [a] -> Maybe (CommaSep1' a)
listToCommaSep1' [] = Nothing
listToCommaSep1' [a] = Just (CommaSepOne1' a Nothing)
listToCommaSep1' (a:as) =
  CommaSepMany1' a [Space] <$> listToCommaSep1' as

instance HasTrailingWhitespace s => HasTrailingWhitespace (CommaSep1' s) where
  trailingWhitespace =
    lens
      (\case
         CommaSepOne1' a b -> fromMaybe (a ^. trailingWhitespace) b
         CommaSepMany1' _ _ a -> a ^. trailingWhitespace)
      (\cs ws ->
         case cs of
           CommaSepOne1' a b ->
             CommaSepOne1'
               (fromMaybe (a & trailingWhitespace .~ ws) $ b $> coerce a)
               (b $> ws)
           CommaSepMany1' a b c ->
             CommaSepMany1' (coerce a) b (c & trailingWhitespace .~ ws))
