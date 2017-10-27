{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
module Language.Python.Expr.AST.StringContent
  ( StringContent
  , StringInside(..)
  , _StringContent
  , stringContent
  , fromList
  , toString
  , AsChar(..)
  )
where

import Prelude (error)
import Papa hiding (cons, empty, snoc)
import qualified Papa as P (snoc)

import Control.Monad

import Language.Python.AST.Symbols
import Language.Python.Expr.AST.EscapeSeq

newtype StringContent b a
  = StringContent
  { _tripleString_value :: [Either EscapeSeq a] }
  deriving (Eq, Show, Ord)

stringContent :: Fold (StringContent a b) (Either EscapeSeq b)
stringContent f (StringContent bs) =
  StringContent <$> traverse f bs

class AsChar s where
  _Char :: Prism' Char s

instance AsChar Char where
  _Char = id

empty_impl :: StringContent a b
empty_impl = StringContent []

cons_impl
  :: forall a b
   . ( StringInside a
     , AsChar b
     )
  => Either EscapeSeq b
  -> StringContent a b
  -> Maybe (StringContent a b)
cons_impl (Right a) (StringContent bs) =
  case bs of
    [] -> singleton (Right a)
    (Left b' : bs') ->
      case _Escape # b' of
        [] -> error "cons_impl: escape sequence serialized to empty string"
        (x:xs) -> case [review _Char a, x] ^? _Escape of
          Nothing -> Just . StringContent $ Right a : bs
          Just a' -> do
            xs' <-
              (xs ^? _StringContent) :: Maybe (StringContent a b)

            StringContent bs'' <- xs' `append` StringContent bs'
            pure . StringContent $ Left a' : bs''
    (Right b' : bs')
      | Just b'' <-
        (review _Char <$> [a, b']) ^? _Escape
      -> Just . StringContent $ Left b'' : bs'
      | otherwise -> Just . StringContent $ Right a : bs
cons_impl a (StringContent bs) = Just . StringContent $ a : bs


append_impl
  :: ( AsChar b
     , StringInside a
     )
  => StringContent a b
  -> StringContent a b
  -> Maybe (StringContent a b)
append_impl (StringContent as) (StringContent bs) =
  case unsnoc as of
    Nothing -> Just $ StringContent bs
    Just (_, Left _) -> Just . StringContent $ as <> bs
    Just (as', Right a') ->
      case bs of
        (Left _ : _) -> Just . StringContent $ as <> bs
        [b'] -> snoc (StringContent as) b'
        (Right b' : bs') ->
          case (review _Char <$> [a', b']) ^? _Escape of
            Nothing -> Just . StringContent $ as <> bs
            Just b'' -> Just . StringContent $ as' <> (Left b'' : bs')
        _ -> Just $ StringContent as

_StringContent
  :: ( StringInside a
     , AsChar b
     )
  => Prism' String (StringContent a b)
_StringContent =
  prism'
    toString
    (foldrM cons empty <=< traverse (fmap Right . preview _Char))

class StringInside a where
  empty :: StringContent a b

  cons
    :: AsChar b
    => Either EscapeSeq b
    -> StringContent a b
    -> Maybe (StringContent a b)

  snoc
    :: AsChar b
    => StringContent a b
    -> Either EscapeSeq b
    -> Maybe (StringContent a b)

  singleton
    :: AsChar b
    => Either EscapeSeq b
    -> Maybe (StringContent a b)

  append
    :: AsChar b
    => StringContent a b
    -> StringContent a b
    -> Maybe (StringContent a b)

instance StringInside SingleQuote where
  empty = empty_impl
  cons = cons_impl

  singleton (Right a) =
    case _Char # a of
      '\'' -> Nothing
      '\\' -> Nothing
      _ -> Just $ StringContent [Right a]
  singleton a = Just $ StringContent [a]

  snoc (StringContent []) a = singleton a
  snoc (StringContent as) (Right a) =
    case _Char # a of
      '\'' -> Nothing
      '\\' -> Nothing
      _ -> Just . StringContent . P.snoc as $ Right a
  snoc (StringContent as) a = Just . StringContent $ P.snoc as a

  append = append_impl


instance StringInside DoubleQuote where
  empty = empty_impl
  cons = cons_impl

  singleton (Right a) =
    case _Char # a of
      '"' -> Nothing
      '\\' -> Nothing
      _ -> Just $ StringContent [Right a]
  singleton a = Just $ StringContent [a]

  snoc (StringContent []) a = singleton a
  snoc (StringContent as) (Right a) =
    case _Char # a of
      '"' -> Nothing
      '\\' -> Nothing
      _ -> Just . StringContent . P.snoc as $ Right a
  snoc (StringContent as) a = Just . StringContent $ P.snoc as a

  append = append_impl

toString :: AsChar a => StringContent b a -> String
toString (StringContent as) =
  foldr
    (\case
        Left a -> (review _Escape a <>)
        Right a -> (_Char # a :))
    ""
    as

fromList
  :: ( StringInside b
     , AsChar a
     )
  => [Either EscapeSeq a]
  -> Maybe (StringContent b a)
fromList = foldrM cons empty
