{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Control.Lens.Error
    (
    -- * Actions
      examine
    , examineList
    , preexamine
    , trySet
    , tryModify
    , tryModify'

    -- * Operators
    , (^&.)
    , (^&..)
    , (^&?)
    , (.&~)
    , (%&~)
    , (%%&~)

    -- * Failing
    , fizzleWhen
    , fizzleUnless
    , fizzleWith
    , fizzleWithWhen
    , fizzleWithUnless
    , maybeFizzleWith
    , orFizzle
    , orFizzleWith

    -- * Adjusting Errors
    , adjustingErrors
    , adjustingErrorsWith

    -- * Types
    , Fizzler
    , Fizzler'

    -- * Classes
    , LensFail(..)

    -- * Re-exports
    , module Data.Either.Validation
    ) where

import Control.Lens.Error.Internal.LensFail
import Control.Lens
import Data.Either.Validation
import Data.Monoid

-- | Represents a lens-like which may fail with an error of type @e@
type Fizzler e s t a b = forall f. (LensFail e f, Applicative f) => LensLike f s t a b

-- | Represents a simple 'Fizzler'
type Fizzler' e s a = Fizzler e s s a a

-- | Cause the current traversal to fizzle with a failure when the focus matches a predicate
--
-- >>> ("hi", [1, 2, 3, 4]) ^&.. _2 . traversed . fizzleWhen ["failure"] even :: ([String], [Int])
-- (["failure","failure"],[1,3])
fizzleWhen :: e -> (s -> Bool) -> Fizzler' e s s
fizzleWhen e check f s | check s = fizzle e
                       | otherwise = f s

-- | Cause the current traversal to fizzle with a failure when the focus fails a predicate
--
-- >>> ("hi", [1, 2, 3, 4]) ^&.. _2 . traversed . fizzleUnless ["failure"] even
-- (["failure","failure"],[2,4])
fizzleUnless :: e -> (s -> Bool) -> Fizzler' e s s
fizzleUnless e check = fizzleWhen e (not . check)

-- | Given a function which might produce an error, fizzle  on 'Just', pass through on 'Nothing'
--
-- > >>> let p x
-- > >>> | even x    = Just [show x <> " was even"]
-- > >>> | otherwise = Nothing
-- > >>> ("hi", [1, 2, 3, 4]) ^&.. _2 . traversed . maybeFizzleWith p
-- > (["2 was even","4 was even"],[1,3])
maybeFizzleWith :: (s -> Maybe e) -> Fizzler' e s s
maybeFizzleWith check f s =
    case check s of
        Nothing -> f s
        Just e -> fizzle e

-- | Fizzle using the given error builder when the focus matches a predicate
--
-- >>> let p x = [show x <> " was even"]
-- >>> ("hi", [1, 2, 3, 4]) ^&.. _2 . traversed . fizzleWithWhen p even
-- (["2 was even","4 was even"],[1,3])
fizzleWithWhen :: (s -> e) -> (s -> Bool) -> Fizzler' e s s
fizzleWithWhen mkErr check f s
  | check s = fizzle $ mkErr s
  | otherwise = f s

-- | Fizzle using the given error builder when the focus fails a predicate
--
-- >>> let p x = [show x <> " was even"]
-- >>> ("hi", [1, 2, 3, 4]) ^&.. _2 . traversed . fizzleWithUnless p odd
-- (["2 was even","4 was even"],[1,3])
fizzleWithUnless :: (s -> e) -> (s -> Bool) -> Fizzler' e s s
fizzleWithUnless mkErr check = fizzleWithWhen mkErr (not . check)

-- | Always fizzle with the given error builder
-- >>> let p x = [show x]
-- >>> ("hi", [1, 2, 3, 4]) ^&.. _2 . traversed . fizzleWith p
-- (["1","2","3","4"],[])
fizzleWith :: (s -> e) -> Fizzler e s t a b
fizzleWith mkErr _ s = fizzle (mkErr s)

-- | Fail with the given error when the provided traversal produces no elements.
--
-- >>> ("hi", [1, 2, 3, 4]) ^&.. (_2 . traversed . filtered (> 10)) `orFizzle` ["nothing over 10"]
-- (["nothing over 10"],[])
orFizzle ::
  (LensFail e f, Applicative f) =>
  Traversing (->) f s t a b -> e -> LensLike f s t a b
orFizzle l e = orFizzleWith l (const e)

-- | Fail using the given error builder when the provided traversal produces no elements.
--
-- >>> ("hi", [1, 2, 3, 4]) ^&.. (_2 . traversed . filtered (> 10)) `orFizzleWith` (\(_, xs) -> ["searched " <> show (length xs) <> " elements, no luck"])
-- (["searched 4 elements, no luck"],[])
orFizzleWith ::
  (LensFail e f, Applicative f) =>
  Traversing (->) f s t a b -> (s -> e) -> LensLike f s t a b
orFizzleWith l mkErr = l `failing` fizzleWith mkErr

infixl 8 ^&.
-- | Operator alias of 'examine'
--
-- View the focus of a lens or traversal over a monoid. Returns the element and the monoidal
-- sum of any errors encountered. Analogous to '^.' with error collection.
--
-- >>> ("hi", [1, 2, 3, 4]) ^&. _2 . traversed . fizzleWithWhen (\n -> [show n]) even . to (:[])
-- (["2","4"],[1,3])
(^&.) :: Monoid e => s -> Getting (e, a) s a -> (e, a)
(^&.) s l = examine l s

infixl 8 ^&..
-- | Operator alias of 'examineList'
--
-- View the focuses of a traversal as a list.
-- Returns the elements and the monoidal sum of any errors encountered. Analogous to '^..'
-- with error collection.
--
-- >>> ("hi", [1, 2, 3, 4]) ^&..  (_2 . traversed . fizzleWithWhen (\n -> [show n]) even)
-- (["2","4"],[1,3])
(^&..) :: Monoid e => s -> Getting (e, [a]) s a -> (e, [a])
(^&..) s l = examineList l s

-- | See also '^&.'
--
-- View the focus of a lens or traversal over a monoid. Returns the element and the monoidal
-- sum of any errors encountered. Analogous to '^.' with error collection.
--
-- >>> examine (_2 . traversed . fizzleWithWhen (\n -> [show n]) even . to (:[])) ("hi", [1, 2, 3, 4])
-- (["2","4"],[1,3])
examine :: Monoid e => Getting (e, a) s a -> s -> (e, a)
examine l = getConst . l (Const . (mempty,))

-- | See also '^&..'
--
-- View the focuses of a traversal as a list.
-- Returns the elements and the monoidal sum of any errors encountered. Analogous to '^..'
-- with error collection.
--
-- >>> examineList ((_2 . traversed . fizzleWithWhen (\n -> [show n]) even)) ("hi", [1, 2, 3, 4])
-- (["2","4"],[1,3])
examineList :: Monoid e => Getting (e, [a]) s a -> s -> (e, [a])
examineList l = getConst . l (Const . (mempty,) . (:[]))

infixl 8 ^&?
-- | Operator alias of 'preexamine'
--
-- Find the first element of a traversal; or return all errors found along the way.
--
-- >>> [1, 2, 3, 4] ^&? traversed . fizzleWhen ["odd"] odd
-- Success 2
--
-- >>> [1, 2, 3, 4] ^&? traversed . fizzleWithWhen (\s -> [show s <> " is too small"]) (<10)
-- Failure ["1 is too small","2 is too small","3 is too small","4 is too small"]
(^&?) :: Monoid e => s -> Getting (e, First a) s a -> Validation e a
(^&?) s l = unpack . getConst .  l (Const . (mempty,) . First . Just) $ s
  where
    unpack (_, First (Just a)) = Success a
    unpack (e, First (Nothing)) = Failure e

-- | See also '^&?'
--
-- Find the first element of a traversal; or return all errors found along the way.
--
-- >>> preexamine (traversed . fizzleWhen ["odd"] odd) [1, 2, 3, 4]
-- Success 2
--
-- >>> preexamine (traversed . fizzleWithWhen (\s -> [show s <> " is too small"]) (<10)) [1, 2, 3, 4]
-- Failure ["1 is too small","2 is too small","3 is too small","4 is too small"]
preexamine :: Monoid e => Getting (e, First a) s a -> s -> Validation e a
preexamine l s = s ^&? l

infixl 8 .&~
-- | Operator alias of 'trySet
--
-- Set the focus of a lens/traversal. Returns a monoidal summary of failures or the altered
-- structure.
--
-- >>> ("hi", [1, 2, 3, 4]) & _2 . ix 1 . fizzleWhen ["shouldn't fail"] (const False) .&~ 42
-- Success ("hi",[1,42,3,4])
--
-- >>> ("hi", [1, 2, 3, 4]) & _2 . ix 1 . fizzleWithWhen (\n -> [n]) even .&~ 42
-- Failure [2]
(.&~) :: LensLike (Validation e) s t a b -> b -> s -> Validation e t
(.&~) l b s = s & l %%~ Success . const b

-- | See also '.&~'
--
-- Set the focus of a lens/traversal. Returns a monoidal summary of failures or the altered
-- structure.
--
-- >>> trySet (_2 . ix 1 . fizzleWhen ["shouldn't fail"] (const False)) 42 ("hi", [1, 2, 3, 4])
-- Success ("hi",[1,42,3,4])
--
-- >>> trySet  (_2 . ix 1 . fizzleWithWhen (\n -> [n]) even) 42 ("hi", [1, 2, 3, 4])
-- Failure [2]
trySet :: LensLike (Validation e) s t a b -> b -> s -> Validation e t
trySet = (.&~)

infixl 8 %&~
-- | Operator alias of 'tryModify'
--
-- Modify the focus of a lens/traversal. Returns a monoidal summary of failures or the altered
-- structure.
--
-- >>> ("hi", [1, 2, 3, 4]) & _2 . traversed . fizzleWhen ["shouldn't fail"] (const False) %&~ (*100)
-- Success ("hi",[100,200,300,400])
--
-- >>> ("hi", [1, 2, 3, 4]) & _2 . traversed . fizzleWithWhen (\n -> [n]) even %&~ (*100)
-- Failure [2,4]
(%&~) :: LensLike (Validation e) s t a b -> (a -> b) -> s -> Validation e t
(%&~) l f s = s & l %%~ Success . f

-- | See also '%&~'
--
-- Modify the focus of a lens/traversal. Returns a monoidal summary of failures or the altered
-- structure.
--
-- >>> tryModify  (_2 . traversed . fizzleWhen ["shouldn't fail"] (const False)) (*100) ("hi", [1, 2, 3, 4])
-- Success ("hi",[100,200,300,400])
--
-- >>> tryModify  (_2 . traversed . fizzleWithWhen (\n -> [n]) even) (*100) ("hi", [1, 2, 3, 4])
-- Failure [2,4]
tryModify :: LensLike (Validation e) s t a b -> (a -> b) -> s -> Validation e t
tryModify l f s = s & l %&~  f

infixl 8 %%&~
-- | Operator alias of 'tryModify''
--
-- Modify the focus of a lens/traversal with a function which may fail.
-- Returns a monoidal summary of failures or the altered structure.
--
-- The definition of this function is actually just:
--
-- > (%%&~) = (%%~)
--
-- But this combinator is provided for discoverability, completeness, and hoogle-ability.
--
-- >>> ("hi", [1, 2, 3, 4]) & _2 . traversed . fizzleWithWhen (\n -> [n]) even %%&~ Success . (*100)
-- Failure [2,4]
-- >>> ("hi", [1, 2, 3, 4]) & _2 . traversed %%&~ (\n -> Failure [show n <> " failed"])
-- Failure ["1 failed","2 failed","3 failed","4 failed"]
(%%&~) :: LensLike (Validation e) s t a b -> (a -> Validation e b) -> s -> Validation e t
(%%&~) = (%%~)

-- | See also '%%&~'
--
-- Modify the focus of a lens/traversal with a function which may fail.
-- Returns a monoidal summary of failures or the altered structure.
--
-- >>> tryModify' (_2 . traversed . fizzleWithWhen (\n -> [n]) even) (Success . (*100)) ("hi", [1, 2, 3, 4])
-- Failure [2,4]
-- >>> tryModify' (_2 . traversed) (\n -> Failure [show n <> " failed"]) ("hi", [1, 2, 3, 4])
-- Failure ["1 failed","2 failed","3 failed","4 failed"]
tryModify' :: LensLike (Validation e) s t a b -> (a -> Validation e b) -> s -> Validation e t
tryModify' l f s = s & l %%~ f


-- | Adjust any errors which occur in the following branch.
-- Note that we can't change the error type, but this can be helpful for adding context
-- to errors if they occur at a position without enough context.
--
-- This is does nothing when no errors occur.
--
-- >>> [1, 2, 3, 4 :: Int] ^&.. traversed . fizzleWhen ["got 4"] (== 4) . adjustingErrors (fmap (<> "!")) . fizzleWhen ["got 3"] (== 3)
-- (["got 3!","got 4"],[1,2])
adjustingErrors :: (e -> e) -> Fizzler' e s s
adjustingErrors addCtx f s = alterErrors addCtx (f s)

-- | Adjust any errors which occur in the following branch, using the value available at
-- the current position to add context..
-- Note that we can't change the error type, but this can be helpful for adding context
-- to errors if they occur at a position without enough context.
--
-- This is does nothing when no errors occur.
--
-- >>> [1, 2, 3, 4 :: Int] ^&.. traversed . fizzleWhen ["got 4"] (== 4) . adjustingErrorsWith (\n -> fmap (\e -> show n <> ": " <> e)) . fizzleWhen ["fail"] (== 3)
-- (["3: fail","got 4"],[1,2])
adjustingErrorsWith :: (s -> e -> e) -> Fizzler' e s s
adjustingErrorsWith addCtx f s = alterErrors (addCtx s) (f s)
