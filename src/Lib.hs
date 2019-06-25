{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module Lib where

import Control.Lens
import Data.Monoid
-- import Data.Either.Validation

class CanFail e f where
  throw :: e -> f a

instance CanFail e (MyValidator e) where
  throw e = Failure e

instance CanFail e (Const (MyValidator e a)) where
  throw e = Const (throw e)

instance Monoid m => CanFail e (Const m) where
  throw _ = Const mempty

data MyValidator e a = Failure e | Success a
  deriving (Show, Eq, Functor)

instance Semigroup e => Applicative (MyValidator e) where
  pure a = Success a
  Failure e <*> Failure e' = Failure (e <> e')
  Failure e <*> Success _ = Failure e
  Success _ <*> Failure e = Failure e
  Success f <*> Success a = Success (f a)

instance (Semigroup e, Semigroup a) => Semigroup (MyValidator e a) where
  Failure e <> Failure e' = Failure (e <> e')
  Success _ <> Failure e = Failure e
  Failure e <> Success _ = Failure e
  Success a <> Success a' = Success (a <> a')

instance (Semigroup e, Monoid a) => Monoid (MyValidator e a) where
  mempty = Success mempty

verify :: CanFail e f => (s -> Maybe e) -> LensLike' f s s
verify check f s =
    case check s of
        Nothing -> f s
        Just e -> throw e

viewSafe :: LensLike (Const (MyValidator e [a])) s t a b -> s -> MyValidator e [a]
viewSafe l s = getConst $ (l %%~ Const . Success . pure) s

overSafe :: (a -> MyValidator e b) -> LensLike (MyValidator e) s t a b -> s -> MyValidator e t
overSafe f l s = (l %%~ f) s

badData :: (String, [Int])
badData = ("hi", [1, 2, 3, 4])

goodData :: (String, [Int])
goodData = ("hi", [2, 4])


traverseEvenNums :: (Applicative f, CanFail [String] f) => LensLike' f (String, [Int]) Int
traverseEvenNums =  _2 . traversed . verify checkEven
  where
    checkEven x | even x = Nothing
                | otherwise = Just [show x <> " is not an even number"]



-- λ> viewSafe traverseEvenNums badData
-- Failure ["1 is not an even number","3 is not an even number"]
-- λ> viewSafe traverseEvenNums goodData
-- Success [2,4]
-- λ> overSafe (Success . (*10)) traverseEvenNums goodData
-- Success ("hi",[20,40])
-- λ> overSafe (Success . (*10)) traverseEvenNums badData
-- Failure ["1 is not an even number","3 is not an even number"]



traverseEvenNums' :: LensLike' (Either String) [Int] Int
traverseEvenNums' f xs = traverse go xs
    where
      go x | even x = f x
           | otherwise = Left (show x <> " is not an even number")

-- λ> ("hi", [1, 2, 3]) & _2 . traverseEvenNums' %%~ pure . (*100)
-- Left "1 is not an even number"
-- λ> ("hi", [2, 4]) & _2 . traverseEvenNums' %%~ pure . (*100)
-- Right ("hi",[200,400])
