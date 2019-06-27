{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Lens
import Control.Lens.Error
import Test.Hspec
import Data.Tree
import Data.Tree.Lens

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

    describe "preexamine ^&?" $ do
        it "Should find first success or return all errors" $ do
            let prismError p name = p `orFizzleWith` (\v -> ["Value " <> show v <> " didn't match: " <> name])
            let _R = prismError _Right "_Right"
            ([Left (1 :: Int), Left 2, Right (3 :: Int)] ^&? traversed . _R)
              `shouldBe` (Success 3)
            ([Left (1 :: Int), Left 2, Left 3] ^&? traversed . _R)
              `shouldBe` (Failure [ "Value Left 1 didn't match: _Right"
                                  , "Value Left 2 didn't match: _Right"
                                  , "Value Left 3 didn't match: _Right"])
    describe "trySet .&~" $ do
        it "should set successfully" $ do
            (numbers & _2 . ix 1 . fizzleWhen ["shouldn't fail"] (const False) .&~ 42)
              `shouldBe` Success ("hi",[1,42,3,4])
        it "should return failures" $ do
            (numbers & _2 . ix 1 . fizzleWithWhen (\n -> [n]) even .&~ 42)
              `shouldBe` Failure [2]

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
        it "should collect all failures if anything fails" $ do
            (numbers & _2 . traversed . fizzleWithWhen (\n -> [n]) even %&~ (*100))
              `shouldBe` Failure [2, 4]

    describe "tryModify' %%&~" $ do
        it "should edit successfully with no assertions" $ do
            (numbers & _2 . traversed %%&~ Success . (*100))
              `shouldBe` (Success ("hi", [100, 200, 300, 400]) :: Validation () (String, [Int]))
        it "should edit successfully through valid assertions" $ do
            (numbers & _2 . traversed . fizzleWhen ["shouldn't fail"] (const False) %%&~ Success . (*100))
              `shouldBe` (Success ("hi", [100, 200, 300, 400]))
        it "should return failures" $ do
            (numbers & _2 . traversed . fizzleWithWhen (\n -> [n]) (const True) %%&~ Success . (*100))
              `shouldBe` Failure [1, 2, 3, 4]
        it "should collect all failures if anything fails" $ do
            (numbers & _2 . traversed . fizzleWithWhen (\n -> [n]) even %%&~ Success . (*100))
              `shouldBe` Failure [2, 4]
        it "should fail if the function fails" $ do
            (numbers & _2 . traversed %%&~ (\n -> Failure [show n <> " failed"]))
              `shouldBe` (Failure ["1 failed","2 failed","3 failed","4 failed"] :: Validation [String] (String, [Int]))

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
    describe "orFizzle" $ do
        it "should always fizzle using the error builder" $ do
            numbers ^&.. (_2 . traversed . filtered (> 10)) `orFizzle` ["nothing over 10"]
              `shouldBe` (["nothing over 10"], [])
    describe "orFizzleWith" $ do
        it "should always fizzle using the error builder" $ do
            numbers ^&.. (_2 . traversed . filtered (> 10)) `orFizzleWith` (\(_, xs) -> ["searched " <> show (length xs) <> " elements, no luck"])
              `shouldBe` (["searched 4 elements, no luck"], [])
    describe "adjustingErrors" $ do
        it "should alter errors from its sub-branch, but not outside of it" $ do
            [1, 2, 3, 4 :: Int] ^&.. traversed . fizzleWhen ["got 4"] (== 4) . adjustingErrors (fmap (<> "!")) . fizzleWhen ["got 3"] (== 3)
              `shouldBe` (["got 3!", "got 4"], [1, 2])
    describe "adjustingErrorsWith" $ do
        it "should alter errors from its sub-branch, but not outside of it, using the value to construct the error" $ do
            [1, 2, 3, 4 :: Int] ^&.. traversed . fizzleWhen ["got 4"] (== 4) . adjustingErrorsWith (\n -> fmap (\e -> show n <> ": " <> e)) . fizzleWhen ["fail"] (== 3)
              `shouldBe` (["3: fail","got 4"], [1, 2])

    describe "real examples" $ do
        it "tree get success" $ do
            let tree = Node "top" [Node "mid" [Node "bottom" []]]
            let tryIx n = ix n `orFizzleWith` (\xs -> [show n <> " was out of bounds in list: " <> show xs])
            tree ^&.. branches . tryIx 0 . branches . tryIx 0 . root
                `shouldBe` ([],["bottom"])
        it "tree get failure" $ do
            let tree = Node "top" [Node "mid" [Node "bottom" []]]
            let tryIx n = ix n `orFizzleWith` (\xs -> [show n <> " was out of bounds in list: " <> show xs])
            tree ^&.. branches . tryIx 0 . branches . tryIx 10 . root
                `shouldBe` (["10 was out of bounds in list: [Node {rootLabel = \"bottom\", subForest = []}]"],[])
        it "tree set" $ do
            let tree = Node "top" [Node "mid" [Node "bottom" []]] :: Tree String
            let tryIx :: (Applicative f, LensFail [String] f, Show a) => Int -> LensLike' f [a] a
                tryIx n = ix n `orFizzleWith` (\xs -> [show n <> " was out of bounds in list: " <> show xs])
            (tree & branches . tryIx 0 . branches . tryIx 10 . root %&~ (<> "!!"))
                `shouldBe` (Failure ["10 was out of bounds in list: [Node {rootLabel = \"bottom\", subForest = []}]"])


