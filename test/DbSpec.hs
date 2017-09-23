module DbSpec where

-- base
import Data.IORef (newIORef, readIORef)

-- persistent-postgresql
import           Database.Persist.Postgresql (fromSqlKey, runSqlPool)

-- hspec
import Test.Hspec (Spec, after_, describe, it, hspec, runIO, shouldBe)
import Test.QuickCheck (property)

import Db
import Models

spec :: Spec
spec = do
  runIO deleteAllAircraftIO
  after_ deleteAllAircraftIO $ do
    describe "Db tests" $ do

      it "insertOneAircraft" $ do
        let ac1 = Aircraft "SN1" 4
        id1 <- fromSqlKey <$> insertAircraftIO ac1
        nAc <- countAircraftIO
        nAc `shouldBe` 1
      
