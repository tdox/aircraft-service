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
                 
-- import           GHC.Generics ()      --  (Generic)

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

----------------------

getAllAircrafts :: SqlPersistT IO [Entity Aircraft]
getAllAircrafts = selectList [] []

getAircraft :: AircraftId -> SqlPersistT IO (Maybe Aircraft)
getAircraft = get

countAircraft :: SqlPersistT IO Int
countAircraft = count ([] :: [Filter Aircraft])

insertAircraft :: Aircraft -> SqlPersistT IO AircraftId
insertAircraft = insert

replaceAircraft :: AircraftId -> Aircraft -> SqlPersistT IO ()
replaceAircraft = replace

deleteAllAircraft :: SqlPersistT IO ()
deleteAllAircraft = deleteWhere ([] :: [Filter Aircraft])

deleteAircraft :: AircraftId -> SqlPersistT IO ()
deleteAircraft = delete


----------------------

allAircraftsIO :: ConnectionPool -> IO [Entity Aircraft]
allAircraftsIO pool = runSqlPool getAllAircrafts pool

getAircraftIO :: ConnectionPool
              -> AircraftId
              -> IO (Maybe (Aircraft))
getAircraftIO pool iD = runSqlPool (getAircraft iD) pool


countAircraftIO :: ConnectionPool -> IO Int
countAircraftIO pool = runSqlPool countAircraft pool

insertAircraftIO :: ConnectionPool -> Aircraft -> IO AircraftId
insertAircraftIO pool ac = runSqlPool (insertAircraft ac) pool

replaceAircraftIO :: ConnectionPool -> AircraftId -> Aircraft -> IO ()
replaceAircraftIO pool acId ac = runSqlPool (replaceAircraft acId ac) pool

deleteAllAircraftIO :: ConnectionPool -> IO ()
deleteAllAircraftIO pool = runSqlPool deleteAllAircraft pool

deleteAircraftIO :: ConnectionPool -> AircraftId -> IO ()
deleteAircraftIO pool acId = runSqlPool (deleteAircraft acId) pool



--------------------------------------------------------------------------------

data Crud ent entId = Crud {
    cCetAll    :: SqlPersistT IO [Entity ent]
  , cGet       :: entId -> SqlPersistT IO (Maybe ent)
  , cCount     :: SqlPersistT IO Int
  , cInsert    :: ent -> SqlPersistT IO entId
  , cReplace   :: entId -> ent -> SqlPersistT IO ()
  , cDeleteAll :: SqlPersistT IO ()
  , cDelete    :: entId -> SqlPersistT IO ()
  }

acCrud :: Crud Aircraft AircraftId
acCrud =
  Crud (selectList [] [])
       get
       (count ([] :: [Filter Aircraft]))
       insert
       replace
       (deleteWhere ([] :: [Filter Aircraft]))
       delete


modelCrud :: Crud Model ModelId
modelCrud =
  Crud (selectList [] [])
       get
       (count ([] :: [Filter Aircraft]))
       insert
       replace
       (deleteWhere ([] :: [Filter Aircraft]))
       delete



{-
aircraftCrud :: Crud Aircraft AircraftId
aircraftCrud =
  Crud allAircrafts
       getAircraft
       countAircraft
       insertAircraft
       replaceAircraft
       deleteAllAircraft
       deleteAircraft
-}
