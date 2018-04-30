{-# language DataKinds #-}
{-# language MultiParamTypeClasses, FunctionalDependencies #-}
{-# language FlexibleInstances, UndecidableInstances #-}
module Language.Python.Internal.Syntax.Token where

import Control.Lens.Getter ((^.), getting)
import Control.Lens.Lens (Lens, lens)
import Control.Lens.Setter ((.~))
import Data.Function ((&))

import Language.Python.Internal.Syntax.Whitespace

class Token s t | s -> t where
  unvalidate :: s -> t
  whitespaceAfter :: Lens s t [Whitespace] [Whitespace]
  startChar :: s -> Char
  endChar :: s -> Char

instance Token String String where
  unvalidate = id
  whitespaceAfter = lens (const []) const
  startChar = head
  endChar = last

instance (Token s t, Token s' t') => Token (s, s') (t, t') where
  unvalidate (a, b) = (unvalidate a, unvalidate b)

  whitespaceAfter =
    lens
      (\(_, b) -> b ^. getting whitespaceAfter)
      (\(a, b) ws -> (unvalidate a, b & whitespaceAfter .~ ws))

  startChar = startChar . fst
  endChar = endChar . snd
