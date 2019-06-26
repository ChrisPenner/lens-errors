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
              `shouldBe` (Success "1234" :: Accum () String)

        it "should view properly through successful assertions" $ do
            numbers ^&. _2 . traversed . asserting ["shouldn't fail"] (const True) . to show
              `shouldBe` (Success "1234" :: Accum [String] String)

        it "should collect failures when they occur" $ do
            numbers ^&. _2 . traversed . failWithWhen (\n -> [show n]) (const True) . to show
              `shouldBe` (Both ["1", "2", "3", "4"] "":: Accum [String] String)

        it "should collect failures AND successes when they occur" $ do
            numbers ^&. _2 . traversed . failWithWhen (\n -> [show n]) even . to (:[])
              `shouldBe` (Both ["2", "4"] [1, 3] :: Accum [String] [Int])

    describe "viewOrFailList (^&..)" $ do
        it "should view properly through traversals/folds" $ do
            numbers ^&.. _2 . traversed
              `shouldBe` (Success [1, 2, 3, 4] :: Accum () [Int])

        it "should view properly through successful assertions" $ do
            numbers ^&..  (_2 . traversed . asserting ["shouldn't fail"] (const True))
              `shouldBe` (Success [1, 2, 3, 4] :: Accum [String] [Int])

        it "should collect failures when they occur" $ do
            numbers ^&..  (_2 . traversed . failWithWhen (\n -> [show n]) (const True))
              `shouldBe` (Both ["1", "2", "3", "4"] [] :: Accum [String] [Int])

        it "should collect failures AND successes when they occur" $ do
            numbers ^&..  (_2 . traversed . failWithWhen (\n -> [show n]) even)
              `shouldBe` (Both ["2", "4"] [1, 3] :: Accum [String] [Int])

    describe "modifyOrFail %&~" $ do
        it "should edit successfully with no assertions" $ do
            (numbers & _2 . traversed %&~ (*100))
              `shouldBe` (Success ("hi", [100, 200, 300, 400]) :: Accum () (String, [Int]))
        it "should edit successfully through valid assertions" $ do
            (numbers & _2 . traversed . asserting ["shouldn't fail"] (const True) %&~ (*100))
              `shouldBe` (Success ("hi", [100, 200, 300, 400]) :: Accum [String] (String, [Int]))
        it "should return failures" $ do
            (numbers & _2 . traversed . failWith (\n -> [show n])  %&~ (*100))
              `shouldBe` (Success ("hi", [100, 200, 300, 400]) :: Accum [String] (String, [Int]))

