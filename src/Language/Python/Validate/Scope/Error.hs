{-# language DataKinds, KindSignatures #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{-|
Module      : Language.Python.Validate.Scope.Error
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Scope.Error where

import Control.Lens.TH
import Language.Python.Syntax.Expr (Expr)
import Language.Python.Syntax.Ident (Ident)

data ScopeError a
  -- |
  -- Using @nonlocal@ to modify function scopes makes scope checking intractible
  = FoundNonlocal a
  -- |
  -- Using @global@ to add identifiers to the global scope makes scope checking
  -- intractible
  | FoundGlobal a
  -- |
  -- Using @del@ to remove identifiers from scope makes scope checking intractible
  | DeletedIdent a
  -- |
  -- Variable assignments deep in control flow can modify the scope outside
  -- the control flow. For example:
  --
  -- @
  -- if a:
  --     x = 0
  -- else:
  --     pass
  --
  -- print(x)
  -- @
  --
  -- @x@ will be in scope if the @True@ branch was entered, but not if the @False@
  -- branch was entered. This kind of behaviour makes scope checking intractible, so
  -- programs like this are considered scope errors.
  | FoundDynamic a (Ident '[] a)
  -- | An identifier is not in scope
  | NotInScope (Ident '[] a)
  -- |
  -- For loops don't execute in a fresh scope, so if the 'counter' of the loop
  -- shadows a variable, then that variable will be mutated.
  --
  -- e.g.
  --
  -- @
  -- x = 0
  -- for x in 1, 2, 3:
  --    pass
  -- print(x)
  -- @
  --
  -- outputs @3@
  --
  -- This error occurs when we spot this pattern.
  | BadShadowing (Ident '[] a)
  -- |
  -- @
  -- # x is in scope
  -- x.a = 0
  -- @
  --
  -- In the above code, knowing whether @x@ is in scope isn't enough to predict
  -- whether the assignment will fail. Neither is knowing @x@'s current attributes.
  -- If @x@ were an instance of a user-defined class, then the assignment would
  -- succeed, and @x@ would be given the @a@ attribute. If @x@ was a built-in type
  -- like @int@ or @string@, then the assignment would fail with an @AttributeError@.
  --
  -- Sometimes we have enough information to know such an assignment will succeed,
  -- and add extra scope information where necessary. This only works if @x@ is
  -- a function or class definition. But in for any other usage we can't know if the
  -- assignment would be successfull, in which case the 'MissingAttribute' error
  -- is raised.
  | MissingAttribute (Expr '[] a) (Ident '[] a)
  deriving (Eq, Show)

makeClassyPrisms ''ScopeError