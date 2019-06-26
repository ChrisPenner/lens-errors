{-# LANGUAGE TypeApplications #-}
import Control.Lens
import Control.Lens.Error
import Test.Hspec

numbers :: (String, [ Int ])
numbers = ("hi", [1, 2, 3, 4])

main :: IO ()
main = hspec $ do
    describe "examine (^&.)" $ do
        it "should view properly through traversals/folds" $ do
            numbers ^&. _2 . traversed . to show
              `shouldBe` ((), "1234")

        it "should view properly through successful assertions" $ do
            numbers ^&. _2 . traversed . fizzleWhen ["shouldn't fail"] (const False) . to show
              `shouldBe` ([], "1234")

        it "should collect failures when they occur" $ do
            numbers ^&. _2 . traversed . fizzleWithWhen (\n -> [show n]) (const True) . to show
              `shouldBe` (["1", "2", "3", "4"], "")

        it "should collect failures AND successes when they occur" $ do
            numbers ^&. _2 . traversed . fizzleWithWhen (\n -> [show n]) even . to (:[])
              `shouldBe` (["2", "4"], [1, 3])

    describe "examineList (^&..)" $ do
        it "should view properly through traversals/folds" $ do
            numbers ^&.. _2 . traversed
              `shouldBe` ((), [1, 2, 3, 4])

        it "should view properly through successful assertions" $ do
            numbers ^&..  (_2 . traversed . fizzleWhen ["shouldn't fail"] (const False))
              `shouldBe` ([], [1, 2, 3, 4])

        it "should collect failures when they occur" $ do
            numbers ^&..  (_2 . traversed . fizzleWithWhen (\n -> [show n]) (const True))
              `shouldBe` (["1", "2", "3", "4"], [])

        it "should collect failures AND successes when they occur" $ do
            numbers ^&..  (_2 . traversed . fizzleWithWhen (\n -> [show n]) even)
              `shouldBe` (["2", "4"], [1, 3])

    describe "tryModify %&~" $ do
        it "should edit successfully with no assertions" $ do
            (numbers & _2 . traversed %&~ (*100))
              `shouldBe` (Success ("hi", [100, 200, 300, 400]) :: Validation () (String, [Int]))
        it "should edit successfully through valid assertions" $ do
            (numbers & _2 . traversed . fizzleWhen ["shouldn't fail"] (const False) %&~ (*100))
              `shouldBe` (Success ("hi", [100, 200, 300, 400]))
        it "should return failures" $ do
            (numbers & _2 . traversed . fizzleWithWhen (\n -> [n]) (const True) %&~ (*100))
              `shouldBe` Failure [1, 2, 3, 4]
        it "should return both failures and successes" $ do
            (numbers & _2 . traversed . fizzleWithWhen (\n -> [n]) even %&~ (*100))
              `shouldBe` Failure [2, 4]

    describe "fizzleWhen" $ do
        it "should fizzle when predicate is true" $ do
            numbers ^&.. _2 . traversed . fizzleWhen ["failure"] even
              `shouldBe` (["failure", "failure"], [1, 3])
    describe "fizzleUnless" $ do
        it "should fizzle when predicate is false" $ do
            numbers ^&.. _2 . traversed . fizzleUnless ["failure"] even
              `shouldBe` (["failure", "failure"], [2, 4])
    describe "maybeFizzleWith" $ do
        it "should fizzle when returning Just" $ do
            let p x
                 | even x    = Just [show x <> " was even"]
                 | otherwise = Nothing
            numbers ^&.. _2 . traversed . maybeFizzleWith p
              `shouldBe` (["2 was even", "4 was even"], [1, 3])
    describe "fizzleWithWhen" $ do
        it "should fizzle using the error builder when predicate is true" $ do
            let p x = [show x <> " was even"]
            numbers ^&.. _2 . traversed . fizzleWithWhen p even
              `shouldBe` (["2 was even", "4 was even"], [1, 3])
    describe "fizzleWithUnless" $ do
        it "should fizzle using the error builder when predicate is false" $ do
            let p x = [show x <> " was even"]
            numbers ^&.. _2 . traversed . fizzleWithUnless p odd
              `shouldBe` (["2 was even", "4 was even"], [1, 3])
    describe "fizzleWith" $ do
        it "should always fizzle using the error builder" $ do
            let p x = [show x]
            numbers ^&.. _2 . traversed . fizzleWith p
              `shouldBe` (["1", "2", "3", "4"], [] :: [Int])
    describe "fizzleWhenEmpty" $ do
        it "should always fizzle using the error builder" $ do
            numbers ^&.. (_2 . traversed . filtered (> 10)) `fizzleWhenEmpty` ["nothing over 10"]
              `shouldBe` (["nothing over 10"], [])
    describe "fizzleWhenEmptyWith" $ do
        it "should always fizzle using the error builder" $ do
            numbers ^&.. (_2 . traversed . filtered (> 10)) `fizzleWhenEmptyWith` (\(_, xs) -> ["searched " <> show (length xs) <> " elements, no luck"])
              `shouldBe` (["searched 4 elements, no luck"], [])
