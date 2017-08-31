module Language.Python.Parser.SrcInfo where

import Papa

import Text.Trifecta

data SrcInfo
  = SrcInfo
  { _srcCaret :: Caret
  , _srcSpan :: Span
  }
  deriving (Eq, Show)

annotated
  :: ( Monad m
     , Functor f
     , DeltaParsing m
     )
  => m (SrcInfo -> f SrcInfo)
  -> m (f SrcInfo)
annotated m = do
  c <- careting
  f :~ s <- spanned m
  pure . f $ SrcInfo c s
