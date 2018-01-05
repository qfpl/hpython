{-# language GeneralizedNewtypeDeriving #-}
module Text.Parser.Unspaced
  ( Unspaced
  , runUnspaced
  )where

import Papa

import Control.Monad.Trans.Class
import Text.Parser.Char (CharParsing(..))
import Text.Parser.Combinators (Parsing(..))
import Text.Parser.LookAhead (LookAheadParsing(..))
import Text.Parser.Token (TokenParsing(..))
import Text.Trifecta (DeltaParsing(..))

import qualified Text.Parser.Token as Parser (Unspaced(..))

newtype Unspaced m a
  = Unspaced { runUnspaced' :: Parser.Unspaced m a }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadPlus
  , Alternative
  , Parsing
  , CharParsing
  , TokenParsing
  , MonadTrans
  )

runUnspaced :: Unspaced m a -> m a
runUnspaced = Parser.runUnspaced . runUnspaced'

instance (MonadPlus m, DeltaParsing m) => DeltaParsing (Unspaced m) where
  line = Unspaced . Parser.Unspaced $ line
  position = Unspaced . Parser.Unspaced $ position
  slicedWith f =
    Unspaced . Parser.Unspaced .
    slicedWith f .
    Parser.runUnspaced . runUnspaced'

instance LookAheadParsing m => LookAheadParsing (Unspaced m) where
  lookAhead =
    Unspaced . Parser.Unspaced .
    lookAhead .
    Parser.runUnspaced . runUnspaced'
