{-# LANGUAGE TypeApplications #-}
import Control.Lens
import Control.Lens.Error
import Test.Hspec

-- simpleExample :: (String, [Either String Int])
-- simpleExample = ("hi", [Left "one", Right 2, Left "three", Right 4])

numbers :: (String, [ Int ])
numbers = ("hi", [1, 2, 3, 4])

main :: IO ()
main = hspec $ do
    describe "viewOrFail (^&.)" $ do
        it "should view properly through traversals/folds" $ do
            numbers ^&. _2 . traversed . to show
              `shouldBe` ((), "1234")

        it "should view properly through successful assertions" $ do
            numbers ^&. _2 . traversed . fizzleWhen ["shouldn't fail"] (const True) . to show
              `shouldBe` ([], "1234")

        it "should collect failures when they occur" $ do
            numbers ^&. _2 . traversed . fizzleWithWhen (\n -> [show n]) (const True) . to show
              `shouldBe` (["1", "2", "3", "4"], "")

        it "should collect failures AND successes when they occur" $ do
            numbers ^&. _2 . traversed . fizzleWithWhen (\n -> [show n]) even . to (:[])
              `shouldBe` (["2", "4"], [1, 3])

    describe "viewOrFailList (^&..)" $ do
        it "should view properly through traversals/folds" $ do
            numbers ^&.. _2 . traversed
              `shouldBe` ((), [1, 2, 3, 4])

        it "should view properly through successful assertions" $ do
            numbers ^&..  (_2 . traversed . fizzleWhen ["shouldn't fail"] (const True))
              `shouldBe` ([], [1, 2, 3, 4])

        it "should collect failures when they occur" $ do
            numbers ^&..  (_2 . traversed . fizzleWithWhen (\n -> [show n]) (const True))
              `shouldBe` (["1", "2", "3", "4"], [])

        it "should collect failures AND successes when they occur" $ do
            numbers ^&..  (_2 . traversed . fizzleWithWhen (\n -> [show n]) even)
              `shouldBe` (["2", "4"], [1, 3])

    describe "modifyOrFail %&~" $ do
        it "should edit successfully with no assertions" $ do
            (numbers & _2 . traversed %&~ (*100))
              `shouldBe` (Success ("hi", [100, 200, 300, 400]) :: Validation () (String, [Int]))
        it "should edit successfully through valid assertions" $ do
            (numbers & _2 . traversed . fizzleWhen ["shouldn't fail"] (const True) %&~ (*100))
              `shouldBe` (Success ("hi", [100, 200, 300, 400]))
        it "should return failures" $ do
            (numbers & _2 . traversed . fizzleWithWhen (\n -> [n]) (const True) %&~ (*100))
              `shouldBe` Failure [1, 2, 3, 4]
        it "should return both failures and successes" $ do
            (numbers & _2 . traversed . fizzleWithWhen (\n -> [n]) even %&~ (*100))
              `shouldBe` Failure [2, 4]
