{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where


import           Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)

import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
                 
import Database.Persist.Types (Filter)
                 
import           GHC.Generics         (Generic)

import           Config               (Config, getPool)

type OrgId = String

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Aircraft json
    serialNumber String
    modelId      ModelId
    deriving Show
    
  Model json
    code         String
    name         String
    numEngines   Int
    orgId        OrgId
    deriving Show
    
  Specifications json
    maxBaggageVolumeCuIn Int
    maxBaggageLenghtIn   Int
    deriving Show

  AircraftSpecs json
    aircraftId AircraftId
    specsId    SpecificationsId
    UniqueAircraftSpecs aircraftId specsId
    deriving Show

  OrganizationModelSpecs json
    orgId    OrgId
    modelId  ModelId
    specsId  SpecificationsId
--    UniqueOrganizationModelSpecs OrgId ModelId SpecificationsId
    deriving Show

  Performance json
    cuiseMode                String
    flightTimeMin            Int
    timeDistanceSlopeNMPerHr Int
    timeDistanceInterceptMin Int
    specsId                  SpecificationsId
--    UniquePerformance cruiseMode specsId
    deriving Show

|]



deriving instance Eq Aircraft
deriving instance Eq Model
deriving instance Eq Specifications
deriving instance Eq AircraftSpecs
deriving instance Eq OrganizationModelSpecs
deriving instance Eq Performance


  {-
doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool


deleteAllAircraft :: SqlPersistT IO ()
deleteAllAircraft = deleteWhere ([] :: [Filter Aircraft])
  
insertAircraft :: Aircraft -> SqlPersistT IO AircraftId
insertAircraft ac = insert ac
-}
