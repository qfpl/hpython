{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}

{-|
Module      : Data.Validate.Monadic
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Data.Validate.Monadic where

import Data.Functor.Compose (Compose(..))
import Data.Semigroup (Semigroup)
import Data.Validation (Validation(..))

-- | This is not a monad transformer
newtype ValidateM e m a = ValidateM { unValidateM :: Compose m (Validation e) a }
  deriving (Functor, Applicative)

runValidateM :: ValidateM e m a -> m (Validation e a)
runValidateM = getCompose . unValidateM

bindVM :: Monad m => m a -> (a -> ValidateM e m b) -> ValidateM e m b
bindVM m f = ValidateM . Compose $ m >>= getCompose . unValidateM . f

liftVM0 :: (Functor m, Semigroup e) => m a -> ValidateM e m a
liftVM0 m = ValidateM . Compose $ pure <$> m

liftVM1 :: (forall x. m x -> m x) -> ValidateM e m a -> ValidateM e m a
liftVM1 f = ValidateM . Compose . f . getCompose . unValidateM

errorVM :: Applicative m => e -> ValidateM e m a
errorVM = ValidateM . Compose . pure . Failure

errorVM1 :: (Applicative f, Applicative m) => e -> ValidateM (f e) m a
errorVM1 = errorVM . pure
