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

module Data.Validate.Monadic
  ( ValidateM (ValidateM, unValidateM)
  , runValidateM
  , bindVM
  , liftVM0
  , liftVM1
  , errorVM
  , errorVM1
  )
where

import Data.Functor.Compose (Compose(..))
import Data.Semigroup (Semigroup)
import Data.Validation (Validation(..))

-- | The composition of 'Data.Validation.Validation' with another 'Applicative' functor.
--
-- 'Data.Validation.Validation' is not a 'Monad', and 'ValidateM' is not a monad transformer.
-- It is equipped with a useful bind function, but that function does not have
-- the right type to make 'ValidateM' a 'Monad' (besides which it would break
-- the laws)
newtype ValidateM e m a = ValidateM { unValidateM :: Compose m (Validation e) a }
  deriving (Functor, Applicative)

-- | Unwrap a 'ValidateM'
runValidateM :: ValidateM e m a -> m (Validation e a)
runValidateM = getCompose . unValidateM

-- | Bind into a 'ValidateM'. Note that the first parameter is @m a@, not @ValidateM e m a@.
bindVM :: Monad m => m a -> (a -> ValidateM e m b) -> ValidateM e m b
bindVM m f = ValidateM . Compose $ m >>= getCompose . unValidateM . f

-- | Lift into a succeeding validation
liftVM0 :: (Functor m, Semigroup e) => m a -> ValidateM e m a
liftVM0 m = ValidateM . Compose $ pure <$> m

-- | Run a natural transformation across 'ValidateM' to alter @m@
liftVM1 :: (forall x. m x -> m x) -> ValidateM e m a -> ValidateM e m a
liftVM1 f = ValidateM . Compose . f . getCompose . unValidateM

-- | Lift an error into 'ValidateM'
errorVM :: Applicative m => e -> ValidateM e m a
errorVM = ValidateM . Compose . pure . Failure

-- | Lift an error in an 'Applicative' into 'ValidateM'. This is especially
-- useful if you're using list or 'Data.List.NonEmpty.NonEmpty' to collect errors.
errorVM1 :: (Applicative f, Applicative m) => e -> ValidateM (f e) m a
errorVM1 = errorVM . pure
