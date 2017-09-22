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

import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import           Data.Aeson           (FromJSON, ToJSON)
import           Database.Persist.Class (Key, selectList)
import           Database.Persist.Class (deleteWhere, insert)
import           Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import           Database.Persist.Types (Entity)

import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import Database.Persist.Types (Filter)
                 
import           GHC.Generics         (Generic)

import           Config               (Config, getPool)

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
  
insertAircraft :: Aircraft -> SqlPersistT IO AircraftId
insertAircraft ac = insert ac

countAircraft :: SqlPersistT IO Int
countAircraft = do
  (acs :: [Entity Aircraft]) <- selectList [] []
  return $ length acs

allAircrafts :: SqlPersistT IO [Entity Aircraft]
allAircrafts = selectList [] []
