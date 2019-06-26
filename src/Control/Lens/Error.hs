{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Control.Lens.Error
    (
    -- * Actions
      examine
    , examineList
    , tryModify
    , tryModify'

    -- * Operators
    , (^&.)
    , (^&..)
    , (%&~)
    , (%%&~)

    -- * Failing
    , fizzleWhen
    , fizzleUnless
    , maybeFizzleWith
    , fizzleWithWhen
    , fizzleWithUnless
    , fizzleWith
    , fizzleWhenEmpty
    , fizzleWhenEmptyWith

    , LensFail(..)
    , module Data.Either.Validation
    ) where

import Control.Lens.Error.Internal.Accum
import Control.Lens
import Data.Either.Validation

fizzleWhen :: forall e s f. LensFail e f => e -> (s -> Bool) -> LensLike' f s s
fizzleWhen e check f s | check s = throw e
                       | otherwise = f s

fizzleUnless :: forall e s f. LensFail e f => e -> (s -> Bool) -> LensLike' f s s
fizzleUnless e check = fizzleWhen e (not . check)

maybeFizzleWith :: forall e s f. LensFail e f => (s -> Maybe e) -> LensLike' f s s
maybeFizzleWith check f s =
    case check s of
        Nothing -> f s
        Just e -> throw e

fizzleWithWhen :: forall e s f. LensFail e f => (s -> e) -> (s -> Bool)-> LensLike' f s s
fizzleWithWhen mkErr check f s
  | check s = throw $ mkErr s
  | otherwise = f s

fizzleWithUnless :: forall e s f. LensFail e f => (s -> e) -> (s -> Bool)-> LensLike' f s s
fizzleWithUnless mkErr check = fizzleWithWhen mkErr (not . check)

fizzleWith :: forall e s t a b f. LensFail e f => (s -> e) -> LensLike f s t a b
fizzleWith mkErr _ s = throw (mkErr s)

fizzleWhenEmpty :: forall e s t a b f.
  (LensFail e f, Applicative f) =>
  Traversing (->) f s t a b -> e -> LensLike f s t a b
fizzleWhenEmpty l e = fizzleWhenEmptyWith l (const e)

fizzleWhenEmptyWith :: forall e s t a b f.
  (LensFail e f, Applicative f) =>
  Traversing (->) f s t a b -> (s -> e) -> LensLike f s t a b
fizzleWhenEmptyWith l mkErr = l `failing` fizzleWith mkErr


infixl 8 ^&.
(^&.) :: forall e s a. Monoid e => s -> Getting (e, a) s a -> (e, a)
(^&.) s l = examine l s

infixl 8 ^&..
(^&..) :: forall e s a. Monoid e => s -> Getting (e, [a]) s a -> (e, [a])
(^&..) s l = examineList l s

examine :: forall e s a. Monoid e => Getting (e, a) s a -> s -> (e, a)
examine l = getConst . l (Const . (mempty,))

examineList :: forall e s a. Monoid e => Getting (e, [a]) s a -> s -> (e, [a])
examineList l = getConst . l (Const . (mempty,) . (:[]))

infixl 8 %&~
(%&~) :: LensLike (Validation e) s t a b -> (a -> b) -> s -> Validation e t
(%&~) l f s = s & l %%~ Success . f
tryModify :: (a -> b) -> LensLike (Validation e) s t a b -> s -> Validation e t
tryModify f l s = s & l %&~  f

infixl 8 %%&~
(%%&~) :: LensLike (Validation e) s t a b -> (a -> Validation e b) -> s -> Validation e t
(%%&~) = (%%~)
tryModify' :: (a -> Validation e b) -> LensLike (Validation e) s t a b -> s -> Validation e t
tryModify' f l s = s & l %%~ f

