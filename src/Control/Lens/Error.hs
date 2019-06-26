{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module Control.Lens.Error
    ( (^&)
    , (%&~)
    , viewOrFail
    , modifyOrFail
    , asserting
    , checking

    , CanFail(..)
    , Accum(..)
    ) where

import Control.Lens.Error.Internal.Accum

import Control.Lens

asserting :: CanFail e f => e -> (s -> Bool) -> LensLike' f s s
asserting e check f s | check s = f s
                      | otherwise = throw e

checking :: CanFail e f => (s -> Maybe e) -> LensLike' f s s
checking check f s =
    case check s of
        Nothing -> f s
        Just e -> throw e

infixl 8 ^&
(^&) ::  s -> Getting (Accum e [a]) s a -> Accum e [a]
(^&) s l = viewOrFail l s
viewOrFail :: Getting (Accum e [a]) s a -> s -> Accum e [a]
viewOrFail l = getConst . l (Const . Success . (:[]))

infixl 8 %&~
(%&~) :: LensLike (Accum e) s t a b -> (a -> Accum e b) -> s -> Accum e t
(%&~) l f s = modifyOrFail f l s
modifyOrFail :: (a -> Accum e b) -> LensLike (Accum e) s t a b -> s -> Accum e t
modifyOrFail f l s = l f s

badData :: (String, [Int])
badData = ("hi", [1, 2, 3, 4])

goodData :: (String, [Int])
goodData = ("hi", [2, 4])

traverseEvenNums :: (Applicative f, CanFail [String] f) => LensLike' f (String, [Int]) Int
traverseEvenNums =  _2 . traversed . checking checkEven
  where
    checkEven x | even x = Nothing
                | otherwise = Just [show x <> " is not an even number"]

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
