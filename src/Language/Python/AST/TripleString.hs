{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
module Language.Python.AST.TripleString
  ( TripleStringContent
  , TripleStringInside(..)
  , _TripleStringContent
  , tripleStringContent
  , toString
  , AsChar(..)
  , parseTripleStringContentSingle
  , parseTripleStringContentDouble
  )
where

import Prelude (error, undefined)

import Papa hiding (cons, empty, snoc)
import qualified Papa as P (snoc)

import Control.Monad
import Data.Maybe
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead

import Language.Python.AST.EscapeSeq
import Language.Python.AST.Symbols

newtype TripleStringContent b a
  = TripleStringContent
  { _tripleString_value :: [Either EscapeSeq a] }
  deriving (Eq, Show)

tripleStringContent :: Fold (TripleStringContent a b) (Either EscapeSeq b)
tripleStringContent f (TripleStringContent bs) =
  TripleStringContent <$> traverse f bs

class AsChar s where
  _Char :: Prism' Char s

instance AsChar Char where
  _Char = id

empty_impl :: TripleStringContent a b
empty_impl = TripleStringContent []

cons_impl
  :: forall a b
   . ( TripleStringInside a
     , AsChar b
     )
  => Either EscapeSeq b
  -> TripleStringContent a b
  -> Maybe (TripleStringContent a b)
cons_impl (Right a) (TripleStringContent bs) =
  case bs of
    [] -> singleton (Right a)
    (Left b' : bs') ->
      case _Escape # b' of
        [] -> error "cons_impl: escape sequence serialized to empty string"
        (x:xs) -> case [review _Char a, x] ^? _Escape of
          Nothing -> Just . TripleStringContent $ Right a : bs
          Just a' -> do
            xs' <-
              (xs ^? _TripleStringContent) :: Maybe (TripleStringContent a b)

            TripleStringContent bs'' <- xs' `append` TripleStringContent bs'
            pure . TripleStringContent $ Left a' : bs''
    (Right b' : bs')
      | Just b'' <-
        (review _Char <$> [a, b']) ^? _Escape
      -> Just . TripleStringContent $ Left b'' : bs'
      | otherwise -> Just . TripleStringContent $ Right a : bs
cons_impl a (TripleStringContent bs) = Just . TripleStringContent $ a : bs


append_impl
  :: ( AsChar b
     , TripleStringInside a
     )
  => TripleStringContent a b
  -> TripleStringContent a b
  -> Maybe (TripleStringContent a b)
append_impl (TripleStringContent as) (TripleStringContent bs) =
  case unsnoc as of
    Nothing -> Just $ TripleStringContent bs
    Just (_, Left _) -> Just . TripleStringContent $ as <> bs
    Just (as', Right a') ->
      case bs of
        (Left _ : _) -> Just . TripleStringContent $ as <> bs
        [b'] -> snoc (TripleStringContent as) b'
        (Right b' : bs') ->
          case (review _Char <$> [a', b']) ^? _Escape of
            Nothing -> Just . TripleStringContent $ as <> bs
            Just b'' -> Just . TripleStringContent $ as' <> (Left b'' : bs')
        _ -> Just $ TripleStringContent as

_TripleStringContent
  :: ( TripleStringInside a
     , AsChar b
     )
  => Prism' String (TripleStringContent a b)
_TripleStringContent =
  prism'
    toString
    (foldrM cons empty <=< traverse (fmap Right . preview _Char))

class TripleStringInside a where
  empty :: TripleStringContent a b

  cons
    :: AsChar b
    => Either EscapeSeq b
    -> TripleStringContent a b
    -> Maybe (TripleStringContent a b)

  snoc
    :: AsChar b
    => TripleStringContent a b
    -> Either EscapeSeq b
    -> Maybe (TripleStringContent a b)

  singleton
    :: AsChar b
    => Either EscapeSeq b
    -> Maybe (TripleStringContent a b)

  append
    :: AsChar b
    => TripleStringContent a b
    -> TripleStringContent a b
    -> Maybe (TripleStringContent a b)

instance TripleStringInside SingleQuote where
  empty = empty_impl
  cons = cons_impl

  singleton (Right a) =
    case _Char # a of
      '\'' -> Nothing
      '\\' -> Nothing
      _ -> Just $ TripleStringContent [Right a]
  singleton a = Just $ TripleStringContent [a]

  snoc (TripleStringContent []) a = singleton a
  snoc (TripleStringContent as) (Right a) =
    case _Char # a of
      '\'' -> Nothing
      '\\' -> Nothing
      _ -> Just . TripleStringContent . P.snoc as $ Right a
  snoc (TripleStringContent as) a = Just . TripleStringContent $ P.snoc as a

  append = append_impl


instance TripleStringInside DoubleQuote where
  empty = empty_impl
  cons = cons_impl

  singleton (Right a) =
    case _Char # a of
      '"' -> Nothing
      '\\' -> Nothing
      _ -> Just $ TripleStringContent [Right a]
  singleton a = Just $ TripleStringContent [a]

  snoc (TripleStringContent []) a = singleton a
  snoc (TripleStringContent as) (Right a) =
    case _Char # a of
      '"' -> Nothing
      '\\' -> Nothing
      _ -> Just . TripleStringContent . P.snoc as $ Right a
  snoc (TripleStringContent as) a = Just . TripleStringContent $ P.snoc as a

  append = append_impl

toString :: AsChar a => TripleStringContent b a -> String
toString (TripleStringContent as) =
  foldr
    (\case
        Left a -> (review _Escape a <>)
        Right a -> (_Char # a :))
    ""
    as

fromList
  :: ( TripleStringInside b
     , AsChar a
     )
  => [Either EscapeSeq a]
  -> Maybe (TripleStringContent b a)
fromList = foldrM cons empty

parseTripleStringContentSingle
  :: ( AsChar a
     , CharParsing m
     , LookAheadParsing m
     )
  => m a
  -> m b
  -> m (TripleStringContent SingleQuote a)
parseTripleStringContentSingle p term =
  fromJust . fromList <$>
  manyTill
    (try (Left <$> escapeSeq) <|>
     try (Right <$> p))
    (lookAhead term)

parseTripleStringContentDouble
  :: ( AsChar a
     , CharParsing m
     , LookAheadParsing m
     )
  => m a
  -> m b
  -> m (TripleStringContent DoubleQuote a)
parseTripleStringContentDouble p term =
  fromJust . fromList <$>
  manyTill
    (try (Left <$> escapeSeq) <|>
     try (Right <$> p))
    (lookAhead term)
