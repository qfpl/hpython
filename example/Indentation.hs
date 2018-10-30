{-# language DataKinds #-}
module Indentation where

import Control.Lens.Setter ((.~))
import Control.Lens.Plated (transform)
import GHC.Natural (Natural)

import Language.Python.Optics
import Language.Python.Syntax.Statement (Statement)
import Language.Python.Syntax.Whitespace (Whitespace (Space, Tab))

indentSpaces :: Natural -> Statement '[] a -> Statement '[] a
indentSpaces n = transform (_Indent .~ replicate (fromIntegral n) Space)

indentTabs :: Statement '[] a -> Statement '[] a
indentTabs = transform (_Indent .~ [Tab])
