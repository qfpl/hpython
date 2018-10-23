{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{-|
Module      : Language.Python.Internal.Syntax.AugAssign
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Syntax.AugAssign where

import Control.Lens.Lens (lens)

import Language.Python.Internal.Syntax.Whitespace

data AugAssign a
  = PlusEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | MinusEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | StarEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | AtEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | SlashEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | PercentEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | AmpersandEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | PipeEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | CaretEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | ShiftLeftEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | ShiftRightEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | DoubleStarEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  | DoubleSlashEq
  { _augAssignAnn :: a
  , _augAssignWhitespace :: [Whitespace]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (AugAssign a) where
  trailingWhitespace =
    lens _augAssignWhitespace (\a b -> a { _augAssignWhitespace = b })
