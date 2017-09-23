module ApiSpec where

-- base
import Data.IORef (newIORef, readIORef)

import Test.Hspec (Spec, describe, it, hspec, runIO, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
    describe "Test suite works" $ do

      it "passes" $ do
        5 `shouldBe` (5 :: Int)
        
      it "does properties" $ property $ \x ->
        x + 1 > (x :: Int)
        
      it "it1" $ do
        ir1 <- newIORef (5 :: Int)
        ir2 <- newIORef (2 :: Int)
        i1 <- readIORef ir1
        i2 <- readIORef ir2
        i1 + i2 `shouldBe` 7
        i1 - i2 `shouldBe` 3
      

--      it "test1" $

io1 :: IO ()
io1 = hspec spec1

spec1 :: Spec
spec1 = do
  describe "spec1" $ do
    it "it1" $ do
      ir1 <- newIORef (5 :: Int)
      ir2 <- newIORef (2 :: Int)
      i1 <- readIORef ir1
      i2 <- readIORef ir2
      i1 + i2 `shouldBe` 7
      i1 - i2 `shouldBe` 3
