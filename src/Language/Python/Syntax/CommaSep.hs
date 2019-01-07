{-# language LambdaCase #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{-|
Module      : Language.Python.Syntax.CommaSep
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Syntax.CommaSep
  ( Comma(..)
  , CommaSep(..), _CommaSep, csTrailingWhitespace
  , appendCommaSep, maybeToCommaSep, listToCommaSep
  , CommaSep1(..)
  , commaSep1Head, appendCommaSep1, listToCommaSep1, listToCommaSep1'
  , CommaSep1'(..)
  , _CommaSep1'
  )
where

import Control.Lens.Getter ((^.))
import Control.Lens.Iso (Iso, iso)
import Control.Lens.Lens (lens)
import Control.Lens.Setter ((.~))
import Control.Lens.Traversal (Traversal')
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))

import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Whitespace (Whitespace (Space), HasTrailingWhitespace (..))

-- | Items separated by commas, with optional whitespace following each comma
data CommaSep a
  = CommaSepNone
  | CommaSepOne a
  | CommaSepMany a Comma (CommaSep a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | 'Traversal' targeting the trailing whitespace in a comma separated list.
--
-- This can't be an instance of 'HasTrailingWhitespace' because 'CommaSepNone' never
-- has trailing whitespace.
csTrailingWhitespace
  :: HasTrailingWhitespace a
  => Traversal' (CommaSep a) [Whitespace]
csTrailingWhitespace _ CommaSepNone = pure CommaSepNone
csTrailingWhitespace f (CommaSepOne a) = CommaSepOne <$> trailingWhitespace f a
csTrailingWhitespace f (CommaSepMany a (MkComma b) CommaSepNone) =
  (\b' -> CommaSepMany a (MkComma b') CommaSepNone) <$> f b
csTrailingWhitespace f (CommaSepMany a b c) =
  CommaSepMany a b <$> csTrailingWhitespace f c


-- | Convert a maybe to a singleton or nullary 'CommaSep'
maybeToCommaSep :: Maybe a -> CommaSep a
maybeToCommaSep = maybe CommaSepNone CommaSepOne

-- | Convert a list to a 'CommaSep'
--
-- Anywhere where whitespace is ambiguous, this function puts a single space
listToCommaSep :: [a] -> CommaSep a
listToCommaSep [] = CommaSepNone
listToCommaSep [a] = CommaSepOne a
listToCommaSep (a:as) = CommaSepMany a (MkComma [Space]) $ listToCommaSep as

-- | Appends two comma separated values together.
--
-- The provided whitespace is to follow the joining comma which is added
appendCommaSep :: [Whitespace] -> CommaSep a -> CommaSep a -> CommaSep a
appendCommaSep _  CommaSepNone b = b
appendCommaSep _  (CommaSepOne a) CommaSepNone = CommaSepOne a
appendCommaSep ws (CommaSepOne a) (CommaSepOne b) = CommaSepMany a (MkComma ws) (CommaSepOne b)
appendCommaSep ws (CommaSepOne a) (CommaSepMany b c cs) = CommaSepMany a (MkComma ws) (CommaSepMany b c cs)
appendCommaSep ws (CommaSepMany a c cs) b = CommaSepMany a c (appendCommaSep ws cs b)

instance Semigroup (CommaSep a) where
  (<>) = appendCommaSep [Space]

instance Monoid (CommaSep a) where
  mempty  = CommaSepNone
  mappend = (<>)

-- | Non-empty 'CommaSep'
data CommaSep1 a
  = CommaSepOne1 a
  | CommaSepMany1 a Comma (CommaSep1 a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Get the first element of a 'CommaSep1'
commaSep1Head :: CommaSep1 a -> a
commaSep1Head (CommaSepOne1 a) = a
commaSep1Head (CommaSepMany1 a _ _) = a

-- | Appends two non-empty comma separated values together.
--
-- The provided whitespace is to follow the joining comma which is added
appendCommaSep1 :: [Whitespace] -> CommaSep1 a -> CommaSep1 a -> CommaSep1 a
appendCommaSep1 ws a b =
  CommaSepMany1
    (case a of; CommaSepOne1 x -> x;  CommaSepMany1 x _ _  -> x)
    (case a of; CommaSepOne1 _ -> MkComma ws; CommaSepMany1 _ ws' _ -> ws')
    (case a of; CommaSepOne1 _ -> b;  CommaSepMany1 _ _ x  -> x <> b)

instance Semigroup (CommaSep1 a) where
  (<>) = appendCommaSep1 [Space]

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

-- | Convert a 'NonEmpty' to a 'CommaSep1'
--
-- Anywhere where whitespace is ambiguous, this function puts a single space
listToCommaSep1 :: NonEmpty a -> CommaSep1 a
listToCommaSep1 (a :| as) = go (a:as)
  where
    go [] = error "impossible"
    go [x] = CommaSepOne1 x
    go (x:xs) = CommaSepMany1 x (MkComma [Space]) $ go xs

-- | Non-empty 'CommaSep', optionally terminated by a comma
--
-- Assumes that the contents consumes trailing whitespace
data CommaSep1' a
  = CommaSepOne1' a (Maybe Comma)
  | CommaSepMany1' a Comma (CommaSep1' a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Iso to unpack a 'CommaSep'
_CommaSep
  :: Iso
       (Maybe (a, [(Comma, a)], Maybe Comma))
       (Maybe (b, [(Comma, b)], Maybe Comma))
       (CommaSep a)
       (CommaSep b)
_CommaSep = iso toCs fromCs
  where
    toCs :: Maybe (a, [(Comma, a)], Maybe Comma) -> CommaSep a
    toCs Nothing = CommaSepNone
    toCs (Just (a, b, c)) =
      case b of
        [] -> maybe (CommaSepOne a) (\c' -> CommaSepMany a c' CommaSepNone) c
        (d, e):ds -> CommaSepMany a d $ toCs (Just (e, ds, c))

    fromCs :: CommaSep a -> Maybe (a, [(Comma, a)], Maybe Comma)
    fromCs CommaSepNone = Nothing
    fromCs (CommaSepOne a) = Just (a, [], Nothing)
    fromCs (CommaSepMany a b c) =
      case fromCs c of
        Nothing -> Just (a, [], Just b)
        Just (x, y, z) -> Just (a, (b, x) : y, z)

-- | Iso to unpack a 'CommaSep1''
_CommaSep1'
  :: Iso
       (a, [(Comma, a)], Maybe Comma)
       (b, [(Comma, b)], Maybe Comma)
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

-- | Attempt to insert comma separators into a list, which will not be
-- terminated by a comma.
--
-- If the list is empty, 'Nothing' is returned.
listToCommaSep1' :: [a] -> Maybe (CommaSep1' a)
listToCommaSep1' [] = Nothing
listToCommaSep1' [a] = Just (CommaSepOne1' a Nothing)
listToCommaSep1' (a:as) =
  CommaSepMany1' a (MkComma [Space]) <$> listToCommaSep1' as

instance HasTrailingWhitespace s => HasTrailingWhitespace (CommaSep1' s) where
  trailingWhitespace =
    lens
      (\case
         CommaSepOne1' a b -> maybe (a ^. trailingWhitespace) (^. trailingWhitespace) b
         CommaSepMany1' _ _ a -> a ^. trailingWhitespace)
      (\cs ws ->
         case cs of
           CommaSepOne1' a b ->
             CommaSepOne1'
               (fromMaybe (a & trailingWhitespace .~ ws) $ b $> coerce a)
               (b $> MkComma ws)
           CommaSepMany1' a b c ->
             CommaSepMany1' (coerce a) b (c & trailingWhitespace .~ ws))
