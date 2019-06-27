{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Lens.Error.Internal.LensFail where

import Data.Functor.Const
import Data.Either.Validation

class LensFail e f | f -> e where
  fizzle :: e -> f a
  alterErrors :: (e -> e) -> f a -> f a

instance Monoid a => LensFail e (Const (e, a)) where
  fizzle e = Const (e, mempty)
  alterErrors f (Const (e, a) ) = Const (f e, a)

instance LensFail e (Const (Either e a)) where
  fizzle e = Const (fizzle e)
  alterErrors f (Const (Left e)) = Const (Left (f e))
  alterErrors _ fa = fa

instance LensFail e (Const (Validation e a)) where
  fizzle e = Const (fizzle e)
  alterErrors f (Const (Failure e)) = Const (Failure (f e))
  alterErrors _ fa = fa

instance LensFail e (Either e) where
  fizzle e = Left e
  alterErrors f (Left e) = Left (f e)
  alterErrors _ fa = fa

instance LensFail e (Validation e) where
  fizzle e = Failure e
  alterErrors f (Failure e) = Failure (f e)
  alterErrors _ fa = fa
