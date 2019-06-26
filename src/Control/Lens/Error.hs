{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Control.Lens.Error
    ( (^&.)
    , (^&..)
    , (%&~)
    , (%%&~)
    , viewOrFail
    , modifyOrFail
    , modifyOrFail'
    , asserting
    , maybeFailWith
    , failWithWhen
    , failWith

    , LensFail(..)
    , module Data.Either.Validation
    ) where

import Control.Lens.Error.Internal.Accum
import Control.Lens
import Data.Either.Validation

asserting :: forall e s f. LensFail e f => e -> (s -> Bool) -> LensLike' f s s
asserting e check f s | check s = f s
                      | otherwise = throw e

maybeFailWith :: forall e s f. LensFail e f => (s -> Maybe e) -> LensLike' f s s
maybeFailWith check f s =
    case check s of
        Nothing -> f s
        Just e -> throw e

failWithWhen :: forall e s f. LensFail e f => (s -> e) ->  (s -> Bool)-> LensLike' f s s
failWithWhen mkErr check f s
  | check s = throw $ mkErr s
  | otherwise = f s

infixl 8 ^&.
(^&.) :: forall e s a. Monoid e => s -> Getting (e, a) s a -> (e, a)
(^&.) s l = viewOrFail l s

infixl 8 ^&..
(^&..) :: forall e s a. Monoid e => s -> Getting (e, [a]) s a -> (e, [a])
(^&..) s l = viewOrFailList l s
viewOrFail :: forall e s a. Monoid e => Getting (e, a) s a -> s -> (e, a)
viewOrFail l = getConst . l (Const . (mempty,))

viewOrFailList :: forall e s a. Monoid e => Getting (e, [a]) s a -> s -> (e, [a])
viewOrFailList l = getConst . l (Const . (mempty,) . (:[]))

infixl 8 %&~
(%&~) :: LensLike (Validation e) s t a b -> (a -> b) -> s -> Validation e t
(%&~) l f s = s & l %%~ Success . f
modifyOrFail :: (a -> b) -> LensLike (Validation e) s t a b -> s -> Validation e t
modifyOrFail f l s = s & l %&~  f

infixl 8 %%&~
(%%&~) :: LensLike (Validation e) s t a b -> (a -> Validation e b) -> s -> Validation e t
(%%&~) = (%%~)
modifyOrFail' :: (a -> Validation e b) -> LensLike (Validation e) s t a b -> s -> Validation e t
modifyOrFail' f l s = s & l %%~ f

failWith :: forall e s t a b f. LensFail e f => (s -> e) -> LensLike f s t a b
failWith mkErr _ s = throw (mkErr s)

failingWith :: forall e s t a b f.
  (LensFail e f, Applicative f) =>
  Traversing (->) f s t a b -> (s -> e) -> LensLike f s t a b
failingWith l mkErr = l `failing` failWith mkErr
