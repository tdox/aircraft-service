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

import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks
                                      , liftIO
                                      , runReaderT)

import Control.Monad.Reader.Class (ask)
                 
import           Data.Aeson           (FromJSON, ToJSON)

import           Database.Persist.Class (Key, count, deleteWhere, delete, get
                                        , insert, replace, selectList)

import           Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import           Database.Persist.Types (Entity)

import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
                 
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)

                 
import Database.Persist.Types (Filter)
                 
import           GHC.Generics         (Generic)

import           Config               (Config, Environment(..), getPool
                                      , makePool)

import Models

--------------------------------------------------------------------------------
doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool


deleteAllAircraft :: SqlPersistT IO ()
deleteAllAircraft = deleteWhere ([] :: [Filter Aircraft])

deleteAllAircraftIO :: ConnectionPool -> IO ()
deleteAllAircraftIO pool = runSqlPool deleteAllAircraft pool


insertAircraft :: Aircraft -> SqlPersistT IO AircraftId
insertAircraft ac = insert ac

insertAircraftIO :: ConnectionPool -> Aircraft -> IO AircraftId
insertAircraftIO pool ac = runSqlPool (insertAircraft ac) pool


countAircraft :: SqlPersistT IO Int
countAircraft = count ([] :: [Filter Aircraft])

countAircraftIO :: ConnectionPool -> IO Int
countAircraftIO pool = runSqlPool countAircraft pool
  

allAircrafts :: SqlPersistT IO [Entity Aircraft]
allAircrafts = selectList [] []

allAircraftsIO :: ConnectionPool -> IO [Entity Aircraft]
allAircraftsIO pool = runSqlPool allAircrafts pool


getAircraft :: AircraftId -> SqlPersistT IO (Maybe Aircraft)
getAircraft iD = get iD


getAircraftIO :: ConnectionPool
              -> AircraftId
              -> IO (Maybe (Aircraft))
getAircraftIO pool iD = runSqlPool (getAircraft iD) pool
                                      
deleteAircraft :: AircraftId -> SqlPersistT IO ()
deleteAircraft iD = delete iD

deleteAircraftIO :: ConnectionPool -> AircraftId -> IO ()
deleteAircraftIO pool acId = runSqlPool (deleteAircraft acId) pool

replaceAircraft :: AircraftId -> Aircraft -> SqlPersistT IO ()
replaceAircraft acId ac = replace acId ac

replaceAircraftIO :: ConnectionPool -> AircraftId -> Aircraft -> IO ()
replaceAircraftIO pool acId ac = runSqlPool (replaceAircraft acId ac) pool
