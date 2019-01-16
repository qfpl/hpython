{-# language DataKinds #-}
module Language.Python.Syntax.Raw where

-- | 'Raw' represents unvalidated, un-annotated terms. 
type Raw f = f '[] ()