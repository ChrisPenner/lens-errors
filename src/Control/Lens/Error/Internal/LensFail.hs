{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Lens.Error.Internal.LensFail where

import Data.Functor.Const
import Data.Either.Validation

class LensFail e f | f -> e where
  fizzle :: e -> f a

instance Monoid a => LensFail e (Const (e, a)) where
  fizzle e = Const (e, mempty)

instance LensFail e (Const (Either e a)) where
  fizzle e = Const (fizzle e)

instance LensFail e (Const (Validation e a)) where
  fizzle e = Const (fizzle e)

instance LensFail e (Either e) where
  fizzle e = Left e

instance LensFail e (Validation e) where
  fizzle e = Failure e
