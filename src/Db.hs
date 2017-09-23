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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Db where

import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO, runReaderT)
import           Data.Aeson           (FromJSON, ToJSON)
import           Database.Persist.Class (Key, selectList)
import           Database.Persist.Class (deleteWhere, insert)
import           Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import           Database.Persist.Types (Entity)

import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
                 
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)

                 
import Database.Persist.Types (Filter)
                 
import           GHC.Generics         (Generic)

import           Config               (Config, Environment(..), getPool, makePool)

import Models

--------------------------------------------------------------------------------
doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

--runDbPool :: MonadIO m => SqlPersistT IO b -> ConnectionPool -> m b
--runDbPool query pool = runSqlPool query pool


deleteAllAircraft :: SqlPersistT IO ()
deleteAllAircraft = deleteWhere ([] :: [Filter Aircraft])

deleteAllAircraftIO :: IO ()
deleteAllAircraftIO = do
  pool <- makePool Localhost
  runSqlPool deleteAllAircraft pool
  
insertAircraft :: Aircraft -> SqlPersistT IO AircraftId
insertAircraft ac = insert ac

insertAircraftIO :: Aircraft -> IO AircraftId
insertAircraftIO ac = do
  pool <- makePool Localhost
  runSqlPool (insertAircraft ac) pool

countAircraft :: SqlPersistT IO Int
countAircraft = do
  (acs :: [Entity Aircraft]) <- selectList [] []
  return $ length acs

countAircraftIO :: IO Int
countAircraftIO = do
  pool <- makePool Localhost
  runSqlPool countAircraft pool
  

allAircrafts :: SqlPersistT IO [Entity Aircraft]
allAircrafts = selectList [] []

allAircraftsIO :: IO [Entity Aircraft]
allAircraftsIO = do
  pool <- makePool Localhost
  runSqlPool allAircrafts pool
