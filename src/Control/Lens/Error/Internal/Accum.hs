{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Lens.Error.Internal.Accum where

import Data.Functor.Const
import Data.Either.Validation

class LensFail e f | f -> e where
  throw :: e -> f a

instance Monoid a => LensFail e (Const (e, a)) where
  throw e = Const (e, mempty)

instance LensFail e (Const (Either e a)) where
  throw e = Const (throw e)

instance LensFail e (Const (Validation e a)) where
  throw e = Const (throw e)

instance LensFail e (Either e) where
  throw e = Left e

instance LensFail e (Validation e) where
  throw e = Failure e
