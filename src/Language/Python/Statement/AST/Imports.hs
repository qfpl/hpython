{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.Imports where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Identifier
import Language.Python.AST.Keywords
import Language.Python.AST.Symbols
import Language.Python.Statement.AST.DottedName

data ImportStatement a
  = ImportStatementName
  { _importStatementName_value :: ImportName a
  , _importStatement_ann :: a
  }
  | ImportStatementFrom
  { _importStatementFrom_value :: ImportFrom a
  , _importStatement_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ImportFrom a
  = ImportFrom
  { _importFrom_from
    :: Sum
         (Compose
           (Before [Between' [WhitespaceChar] (Either Dot Ellipsis)])
           DottedName)
         (Const (NonEmpty (Either Dot Ellipsis)))
         a
  , _importFrom_import
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Sum
           (Sum
             (Const Asterisk)
             (Compose
               (Between LeftParen RightParen)
               (Compose
                 (Between' [WhitespaceChar])
                 ImportAsNames)))
           ImportAsNames)
         a
  , _importFrom_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ImportFrom a)
deriving instance Show a => Show (ImportFrom a)

data ImportAsNames a
  = ImportAsNames
  { _importAsNames_head :: ImportAsName a
  , _importAsNames_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           ImportAsName)
         a
  , _importAsNames_comma :: Maybe (Between' [WhitespaceChar] Comma)
  , _importAsNames_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ImportAsNames a)
deriving instance Show a => Show (ImportAsNames a)

data ImportAsName a
  = ImportAsName
  { _importAsName_left :: Identifier a
  , _importAsName_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KAs))
           Identifier)
         a
  , _importAsName_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ImportName a
  = ImportName
  { _importName_value
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         DottedAsNames
         a
  , _importName_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ImportName a)
deriving instance Show a => Show (ImportName a)

data DottedAsName a
  = DottedAsName
  { _dottedAsName_left :: DottedName a
  , _dottedAsName_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KAs))
           Identifier)
         a
  , _dottedAsName_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data DottedAsNames a
  = DottedAsNames
  { _dottedAsNames_head :: DottedAsName a
  , _dottedAsNames_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           DottedAsName)
         a
  , _dottedAsNames_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (DottedAsNames a)
deriving instance Show a => Show (DottedAsNames a)

makeLenses ''DottedAsName
deriveEq1 ''DottedAsName
deriveShow1 ''DottedAsName

makeLenses ''ImportAsName
deriveEq1 ''ImportAsName
deriveShow1 ''ImportAsName

makeLenses ''DottedAsNames
deriveEq1 ''DottedAsNames
deriveShow1 ''DottedAsNames

makeLenses ''ImportAsNames
deriveEq1 ''ImportAsNames
deriveShow1 ''ImportAsNames

makeLenses ''ImportStatement
deriveEq1 ''ImportStatement
deriveShow1 ''ImportStatement

makeLenses ''ImportName
deriveEq1 ''ImportName
deriveShow1 ''ImportName

makeLenses ''ImportFrom
deriveEq1 ''ImportFrom
deriveShow1 ''ImportFrom
