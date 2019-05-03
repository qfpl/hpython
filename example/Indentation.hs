{-# language DataKinds #-}
module Indentation where

import Control.Lens.Setter ((.~))
import Control.Lens.Plated (transform)
import GHC.Natural (Natural)

import Language.Python35.Optics
import Language.Python35.Syntax.Statement (Statement)
import Language.Python.Syntax.Whitespace (Whitespace (Space, Tab))

{-

These functions show how we can use Control.Lens.Plated to perform
whole-program transformations.

They're illustrative only, because these functions aren't enough to re-indent
all Python code properly. The _Indent optic is limited in where it can reach. See
Language.Python.Optics.Indents for more info.

-}

indentSpaces :: Natural -> Statement '[] a -> Statement '[] a
indentSpaces n = transform (_Indent .~ replicate (fromIntegral n) Space)

indentTabs :: Statement '[] a -> Statement '[] a
indentTabs = transform (_Indent .~ [Tab])
