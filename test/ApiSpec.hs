module ApiSpec where

-- base
import Control.Concurrent
import Data.IORef (newIORef, readIORef)


-- http-client
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)

-- persistent
import Database.Persist.Types

-- servant-client
import Servant.Client (BaseUrl(BaseUrl), ClientM, Scheme(Http), ServantError, client)


import Test.Hspec (Spec, describe, it, hspec, runIO, shouldBe)
import Test.QuickCheck (property)

import Client
import Ios
import Models
import qualified Models as M


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

      
  describe "write then read some data" $ do
    it "write one aircraft" $ do
      tid <- forkIO Ios.runService
      threadDelay 100000
      mgr <- newManager defaultManagerSettings
      port <- readPort
      let bu = BaseUrl Http "localhost" port ""

      let ac = Aircraft "SN1" 2
      Right id1 <- postAircraftIO mgr bu ac
      eAc <- getAircraftIO mgr bu id1
      eAc `shouldBe` Right ac
      deleteAllIO mgr bu
      killThread tid

    it "write two aircrafts" $ do
      tid <- forkIO Ios.runService
      threadDelay 100000
      mgr <- newManager defaultManagerSettings
      port <- readPort
      let bu = BaseUrl Http "localhost" port ""

      let ac1 = Aircraft "SN1" 2
      Right id1 <- postAircraftIO mgr bu ac1
      let ac2 = Aircraft "SN1" 2
      Right id2 <- postAircraftIO mgr bu ac2

      eAc1 <- getAircraftIO mgr bu id1
      eAc1 `shouldBe` Right ac1
      eAc2 <- getAircraftIO mgr bu id2
      eAc2 `shouldBe` Right ac2

      
      deleteAllIO mgr bu
      killThread tid

      

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

{-
spec2 :: Spec
spec2 = do
  describe "write then read some data" $ do
    tid <- forkIO Ios.io1
    mgr <- newManager defaultManagerSettings
    port <- readPort
    let bu = BaseUrl Http "localhost" port ""

    it "write one aircraft" $ do
      let ac = Aircraft "SN1" 2
      id1 <- postAircraftIO mgr bu ac
      eAc <- getAircraftIO mgr bu id1
      eAc `shouldBe` Right _
      entityVal (fromRight eAc) `shouldBe` ac

    killThread tid
-}


spec2 :: Spec
spec2 = do
  describe "write then read some data" $ do
    it "write one aircraft" $ do
      tid <- forkIO Ios.runService
      threadDelay 100000
      mgr <- newManager defaultManagerSettings
      port <- readPort
      let bu = BaseUrl Http "localhost" port ""

      let ac = Aircraft "SN1" 2
      Right id1 <- postAircraftIO mgr bu ac
      eAc <- getAircraftIO mgr bu id1
      eAc `shouldBe` Right ac
--      fromRight eAc `shouldBe` ac

      killThread tid
      
spec3 :: Spec
spec3 = do
  tid <- runIO $ do
    tid1 <- forkIO Ios.runService
    threadDelay 100000
    return tid1

  runIO $ threadDelay 100000  
  describe "write then read some data" $ do
    it "write one aircraft" $ do
--      tid <- forkIO Ios.runService
--      threadDelay 100000
      mgr <- newManager defaultManagerSettings
      port <- readPort
      let bu = BaseUrl Http "localhost" port ""

      let ac = Aircraft "SN1" 2
      Right id1 <- postAircraftIO mgr bu ac
      eAc <- getAircraftIO mgr bu id1
      eAc `shouldBe` Right ac
--      fromRight eAc `shouldBe` ac

  runIO $ killThread tid

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _) = error "fromRight: is Left"
