{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Lens.Error
    (
    -- * Actions
      examine
    , examineList
    , tryModify
    , tryModify'
    , preexamine

    -- * Operators
    , (^&.)
    , (^&..)
    , (^&?)
    , (%&~)
    , (%%&~)

    -- * Failing
    , fizzleWhen
    , fizzleUnless
    , fizzleWith
    , fizzleWithWhen
    , fizzleWithUnless
    , maybeFizzleWith
    , fizzleWhenEmpty
    , fizzleWhenEmptyWith

    -- * Classes
    , LensFail(..)

    -- * Re-exports
    , module Data.Either.Validation
    ) where

import Control.Lens.Error.Internal.LensFail
import Control.Lens
import Data.Either.Validation
import Data.Monoid

-- | Cause the current traversal to fizzle with a failure when the focus matches a predicate
fizzleWhen :: LensFail e f => e -> (s -> Bool) -> LensLike' f s s
fizzleWhen e check f s | check s = fizzle e
                       | otherwise = f s

-- | Cause the current traversal to fizzle with a failure when the focus fails a predicate
fizzleUnless :: LensFail e f => e -> (s -> Bool) -> LensLike' f s s
fizzleUnless e check = fizzleWhen e (not . check)

-- | Given a function which might produce an error, fizzle  on 'Just', pass through on 'Nothing'
maybeFizzleWith :: LensFail e f => (s -> Maybe e) -> LensLike' f s s
maybeFizzleWith check f s =
    case check s of
        Nothing -> f s
        Just e -> fizzle e

-- | Fizzle using the given error builder when the focus matches a predicate
fizzleWithWhen :: LensFail e f => (s -> e) -> (s -> Bool) -> LensLike' f s s
fizzleWithWhen mkErr check f s
  | check s = fizzle $ mkErr s
  | otherwise = f s

-- | Fizzle using the given error builder when the focus fails a predicate
fizzleWithUnless :: LensFail e f => (s -> e) -> (s -> Bool)-> LensLike' f s s
fizzleWithUnless mkErr check = fizzleWithWhen mkErr (not . check)

-- | Always fizzle with the given error builder
fizzleWith :: LensFail e f => (s -> e) -> LensLike f s t a b
fizzleWith mkErr _ s = fizzle (mkErr s)

-- | Fail with the given error when the provided traversal produces no elements.
fizzleWhenEmpty ::
  (LensFail e f, Applicative f) =>
  Traversing (->) f s t a b -> e -> LensLike f s t a b
fizzleWhenEmpty l e = fizzleWhenEmptyWith l (const e)

-- | Fail using the given error builder when the provided traversal produces no elements.
fizzleWhenEmptyWith ::
  (LensFail e f, Applicative f) =>
  Traversing (->) f s t a b -> (s -> e) -> LensLike f s t a b
fizzleWhenEmptyWith l mkErr = l `failing` fizzleWith mkErr

infixl 8 ^&.
-- | Operator alias of 'examine'
-- View the focus of a lens or traversal over a monoid. Returns the element and the monoidal
-- sum of any errors encountered. Analogous to '^.' with error collection.
(^&.) :: Monoid e => s -> Getting (e, a) s a -> (e, a)
(^&.) s l = examine l s

infixl 8 ^&..
-- | Operator alias of 'examineList'
-- View the focuses of a traversal as a list.
-- Returns the elements and the monoidal sum of any errors encountered. Analogous to '^..'
-- with error collection.
(^&..) :: Monoid e => s -> Getting (e, [a]) s a -> (e, [a])
(^&..) s l = examineList l s

-- | See also '^&.'
-- View the focus of a lens or traversal over a monoid. Returns the element and the monoidal
-- sum of any errors encountered. Analogous to '^.' with error collection.
examine :: Monoid e => Getting (e, a) s a -> s -> (e, a)
examine l = getConst . l (Const . (mempty,))

-- | See also '^&..'
-- View the focuses of a traversal as a list.
-- Returns the elements and the monoidal sum of any errors encountered. Analogous to '^..'
-- with error collection.
examineList :: Monoid e => Getting (e, [a]) s a -> s -> (e, [a])
examineList l = getConst . l (Const . (mempty,) . (:[]))

infixl 8 ^&?
-- | Operator alias of 'preexamine'
-- Find the first element of a traversal; or return all errors found along the way.
(^&?) :: Monoid e => s -> Getting (e, First a) s a -> Validation e a
(^&?) s l = unpack . getConst .  l (Const . (mempty,) . First . Just) $ s
  where
    unpack (_, First (Just a)) = Success a
    unpack (e, First (Nothing)) = Failure e

preexamine :: Monoid e => s -> Getting (e, First a) s a -> Validation e a
preexamine s l = s ^&? l

infixl 8 %&~
-- | Operator alias of 'tryModify'
-- Modify the focus of a lens/traversal. Returns a monoidal summary of failures or the altered
-- structure.
(%&~) :: LensLike (Validation e) s t a b -> (a -> b) -> s -> Validation e t
(%&~) l f s = s & l %%~ Success . f

-- | See also '%&~'
-- Modify the focus of a lens/traversal. Returns a monoidal summary of failures or the altered
-- structure.
tryModify :: (a -> b) -> LensLike (Validation e) s t a b -> s -> Validation e t
tryModify f l s = s & l %&~  f

infixl 8 %%&~
-- | Operator alias of 'tryModify''
-- Modify the focus of a lens/traversal with a function which may fail.
-- Returns a monoidal summary of failures or the altered structure.
--
-- The definition of this function is actually just:
--
-- > (%%&~) = (%%~)
--
-- But this combinator is provided for discoverability, completeness, and hoogle-ability.
(%%&~) :: LensLike (Validation e) s t a b -> (a -> Validation e b) -> s -> Validation e t
(%%&~) = (%%~)

-- | See also '%%&~'
-- Modify the focus of a lens/traversal with a function which may fail.
-- Returns a monoidal summary of failures or the altered structure.
tryModify' :: (a -> Validation e b) -> LensLike (Validation e) s t a b -> s -> Validation e t
tryModify' f l s = s & l %%~ f

