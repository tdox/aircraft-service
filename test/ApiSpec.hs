module ApiSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
    describe "Test suite works" $ do
        it "passes" $ do
            5 `shouldBe` (5 :: Int)
        it "does properties" $ property $ \x ->
            x + 1 > (x :: Int)
