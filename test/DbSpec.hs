{-# LANGUAGE ScopedTypeVariables #-}

module DbSpec where

-- base
import Data.IORef (newIORef, readIORef)

-- persistent-postgresql
import Database.Persist.Postgresql (ConnectionPool, fromSqlKey
                                   , toSqlKey, runSqlPool)

-- hspec
import Test.Hspec (Spec, after_, describe, it, hspec, runIO, shouldBe)
import Test.QuickCheck (property)

import Config
import Db
import Ios
import Models

spec :: Spec
spec = do
  let
    ac1 = Aircraft "SN1" 4
    ac2 = Aircraft "SN2" 2
  (pool :: ConnectionPool)  <- runIO $ do
    cfg <- readConfig
    let pool = getPool cfg
    deleteAllAircraftIO pool
    return pool
  after_ (deleteAllAircraftIO pool) $ do
    describe "Db tests" $ do

      it "insertOneAircraft" $ do
        id1 <- insertAircraftIO pool ac1
        nAc <- countAircraftIO pool
        nAc `shouldBe` 1
        Just ac1' <- getAircraftIO pool id1
        ac1' `shouldBe` ac1
      
      it "insertTwoAircraft" $ do
        id1 <- insertAircraftIO pool ac1
        id2 <- insertAircraftIO pool ac2
        nAc <- countAircraftIO pool
        nAc `shouldBe` 2
        Just ac1' <- getAircraftIO pool id1
        ac1' `shouldBe` ac1
        Just ac2' <- getAircraftIO pool id2
        ac2' `shouldBe` ac2

        mac <- getAircraftIO pool $ toSqlKey 99999
        mac `shouldBe` Nothing
