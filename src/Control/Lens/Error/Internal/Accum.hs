{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Control.Lens.Error.Internal.Accum where

import Data.Functor.Const

data Accum e a = Failure e | Success a | Both e a
  deriving (Show, Eq, Functor)

instance Semigroup e => Applicative (Accum e) where
  pure a = Success a
  Failure e <*> Failure e' = Failure (e <> e')
  Failure e <*> Success _ = Failure e
  Success _ <*> Failure e = Failure e
  Success f <*> Success a = Success (f a)
  Both e f <*> Success a = Both e (f a)
  Both e _ <*> Failure e' = Failure (e <> e')
  Success f <*> Both e a = Both e (f a)
  Failure e <*> Both e' _ = Failure (e <> e')
  Both e f <*> Both e' a = Both (e <> e') (f a)

instance (Semigroup e, Semigroup a) => Semigroup (Accum e a) where
  Failure e <> Failure e' = Failure (e <> e')
  Success a <> Failure e = Both e a
  Failure e <> Success a = Both e a
  Success a <> Success a' = Success (a <> a')
  Both e a <> Failure e' = Both (e <> e') a
  Both e a <> Success a' = Both e (a <> a')
  Failure e <> Both e' a = Both (e <> e') a
  Success a <> Both e a' = Both e (a <> a')
  Both e a <> Both e' a' = Both (e <> e') (a <> a')

instance (Semigroup e, Monoid a) => Monoid (Accum e a) where
  mempty = Success mempty

class CanFail e f where
  throw :: e -> f a

instance CanFail e (Accum e) where
  throw e = Failure e

instance {-# OVERLAPPING #-} CanFail e (Const (Accum e a)) where
  throw e = Const (throw e)

-- -- This allows most folds in lens to still run; just ignoring errors.
instance {-# OVERLAPPABLE #-} Monoid m => CanFail e (Const m) where
  throw _ = Const mempty

