{-# LANGUAGE ScopedTypeVariables #-}

module Api.AircraftSpec where

-- base
import Data.IORef (newIORef, readIORef)

-- persistent-postgresql
import Database.Persist.Postgresql (ConnectionPool, fromSqlKey
                                   , toSqlKey, runSqlPool)

-- hspec
import Test.Hspec (Spec, after_, describe, it, hspec, runIO, shouldBe)
import Test.QuickCheck (property)

-- import Config ()
-- import Db ()
-- import Ios
import Models (Aircraft(Aircraft))

spec :: Spec
spec = do
  let
    ac1 = Aircraft "SN1" $ toSqlKey 1
    ac2 = Aircraft "SN2" $ toSqlKey 2
    ac3 = Aircraft "SN3" $ toSqlKey 3

  runIO $ putStrLn "done"
