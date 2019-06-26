{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
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

    , CanFail(..)
    , Accum(..)
    ) where

import Control.Lens.Error.Internal.Accum

import Control.Lens

asserting :: forall e s f. CanFail e f => e -> (s -> Bool) -> LensLike' f s s
asserting e check f s | check s = f s
                      | otherwise = throw e

maybeFailWith :: forall e s f. CanFail e f => (s -> Maybe e) -> LensLike' f s s
maybeFailWith check f s =
    case check s of
        Nothing -> f s
        Just e -> throw e

failWithWhen :: forall e s f. CanFail e f => (s -> e) ->  (s -> Bool)-> LensLike' f s s
failWithWhen mkErr check f s
  | check s = throw $ mkErr s
  | otherwise = f s


infixl 8 ^&.
(^&.) :: forall e s a.  s -> Getting (Accum e a) s a -> Accum e a
(^&.) s l = viewOrFail l s

infixl 8 ^&..
(^&..) :: forall e s a. s -> Getting (Accum e [a]) s a -> Accum e [a]
(^&..) s l = viewOrFailList l s
viewOrFail :: forall e s a.Getting (Accum e a) s a -> s -> Accum e a
viewOrFail l = getConst . l (Const . Success)

viewOrFailList :: forall e s a.Getting (Accum e [a]) s a -> s -> Accum e [a]
viewOrFailList l = getConst . l (Const . Success . (:[]))

infixl 8 %&~
(%&~) :: LensLike (Accum e) s t a b -> (a -> b) -> s -> Accum e t
(%&~) l f s = s & l %%~ Success . f
modifyOrFail :: (a -> b) -> LensLike (Accum e) s t a b -> s -> Accum e t
modifyOrFail f l s = s & l %&~  f

infixl 8 %%&~
(%%&~) :: LensLike (Accum e) s t a b -> (a -> Accum e b) -> s -> Accum e t
(%%&~) = (%%~)
modifyOrFail' :: (a -> Accum e b) -> LensLike (Accum e) s t a b -> s -> Accum e t
modifyOrFail' f l s = s & l %%~ f

failWith :: forall e s t a b f. CanFail e f => (s -> e) -> LensLike f s t a b
failWith mkErr _ s = throw (mkErr s)

failingWith :: forall e s t a b f.
  (CanFail e f, Applicative f) =>
  Traversing (->) f s t a b -> (s -> e) -> LensLike f s t a b
failingWith l mkErr = l `failing` failWith mkErr

testFailingWith :: Accum [String] [Int]
testFailingWith = goodData ^&.. _2 . traversed . (filtered odd `failingWith` const (["nope"] :: [String]))

testVerify :: Accum [String] [Int]
testVerify = badData ^&.. _2 . traversed . asserting ["not even"] even

testRegular :: [Int]
testRegular = goodData ^.. _2 . traversed . (filtered odd `failingWith` const ("nope" :: String))

testPreview = badData ^? _2 . traversed . asserting ["not odd"] odd


-- generalize :: CanFail f => Accum e a -> f a
-- generalize (Failure e) = throw e
-- generalize (Both e) = throw e

badData :: (String, [Int])
badData = ("hi", [1, 2, 3, 4])

goodData :: (String, [Int])
goodData = ("hi", [2, 4])

traverseEvenNums :: (Applicative f, CanFail [String] f) => LensLike' f (String, [Int]) Int
traverseEvenNums =  _2 . traversed . maybeFailWith checkEven
  where
    checkEven x | even x = Nothing
                | otherwise = Just [show x <> " is not an even number"]

result :: Accum [String] [Int]
result = badData ^&.. traverseEvenNums

-- testing :: Accum String [Int]
-- testing = goodData ^&.. _2 . (traversed . (filtered odd `failing` failWith (\s -> show s <> " is no good!")))

-- -- λ> viewOrFail traverseEvenNums badData
-- -- Failure ["1 is not an even number","3 is not an even number"]
-- -- λ> viewOrFail traverseEvenNums goodData
-- -- Success [2,4]
-- -- λ> overSafe (Success . (*10)) traverseEvenNums goodData
-- -- Success ("hi",[20,40])
-- -- λ> overSafe (Success . (*10)) traverseEvenNums badData
-- -- Failure ["1 is not an even number","3 is not an even number"]



-- traverseEvenNums' :: LensLike' (Either String) [Int] Int
-- traverseEvenNums' f xs = traverse go xs
--     where
--       go x | even x = f x
--            | otherwise = Left (show x <> " is not an even number")

-- -- λ> ("hi", [1, 2, 3]) & _2 . traverseEvenNums' %%~ pure . (*100)
-- -- Left "1 is not an even number"
-- -- λ> ("hi", [2, 4]) & _2 . traverseEvenNums' %%~ pure . (*100)
-- -- Right ("hi",[200,400])
