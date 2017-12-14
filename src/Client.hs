module Client where

-- base
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (void, when)
import Data.Int (Int64)

-- assert
import Control.Exception.Assert (assert)

-- http-client
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)

-- mtl
import Control.Monad.Except (ExceptT, runExceptT)

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, liftIO)

import Database.Persist.Postgresql (fromSqlKey, runSqlPool)


import Servant ( (:>), (:<|>)((:<|>)), Capture , DeleteNoContent, Get
               , JSON, Post, Proxy(Proxy), NoContent(..), ReqBody
               , ServerT, err404, throwError)
       
-- servant-client
import Servant.Client (BaseUrl(BaseUrl), ClientM, Scheme(Http), ServantError
                      , client)



import qualified Db
import Database.Persist.Types (Entity, entityVal)

import Models (Aircraft, Model)
import Api.Aircraft (AircraftAPI)
import Api.Model (ModelAPI)

--------------------------------------------------------------------------------

aircraftAPI :: Proxy AircraftAPI
aircraftAPI = Proxy

--getAllAircrafts ::             Manager -> BaseUrl -> ClientM [Entity Aircraft]
getAllAircrafts ::             ClientM [Entity Aircraft]


getAircraft     :: {-Manager -> BaseUrl -> -} Int64   -> ClientM Aircraft


postAircraft    :: {- Manager -> BaseUrl -> -} Aircraft -> ClientM Int64
--                -> ExceptT ServantError IO Int64
                
deleteAllAircrafts :: {- Manager -> BaseUrl -> -} ClientM NoContent



getAllAircrafts :<|> getAircraft :<|> postAircraft :<|> deleteAllAircrafts =
  client aircraftAPI



{-
getAllAircraftsIO :: Manager
                  -> BaseUrl
                  -> ClientM [Entity Aircraft]
--                  -> IO (Either ServantError [Entity Aircraft])
                  
getAllAircraftsIO mgr bu = getAllAircrafts mgr bu
--getAllAircraftsIO mgr bu = runExceptT $ getAllAircrafts mgr bu


getAircraftIO :: Manager -> BaseUrl -> Int64
              -> ClientM Aircraft
--              -> IO (Either ServantError Aircraft)
              
getAircraftIO mgr bu iD = getAircraft iD mgr bu
--getAircraftIO mgr bu iD = runExceptT $ getAircraft iD mgr bu


postAircraftIO :: Manager -> BaseUrl -> Aircraft
               -> ClientM Int64
--               -> IO (Either ServantError Int64)
               
postAircraftIO m b ac = postAircraft ac m b
--postAircraftIO m b ac = runExceptT $ postAircraft ac m b

--deleteAllAircraftIO :: Manager -> BaseUrl -> IO (Either ServantError NoContent)
deleteAllAircraftIO :: Manager -> BaseUrl -> ClientM NoContent
deleteAllAircraftIO m b = deleteAllAircrafts m b
--deleteAllAircraftIO m b = runExceptT $ deleteAllAircrafts m b
-}

--------------------------------------------------------------------------------

-- data ClientEnv = ClientEnv Manager BaseUrl
-- type Client = ReaderT ClientEnv IO

--------------------------------------------------------------------------------

{-
getAllAircraftsC :: Client (Either ServantError [Entity Aircraft])
getAllAircraftsC = do
  ClientEnv m b <- ask
  liftIO $ runExceptT $ getAllAircrafts m b
  

getAircraftC :: Int64 -> Client (Either ServantError Aircraft)
getAircraftC iD = do
  ClientEnv m b <- ask
  liftIO $ runExceptT $ getAircraft iD m b

postAircraftC :: Aircraft -> Client (Either ServantError Int64)
postAircraftC ac = do
  ClientEnv m b <- ask
  liftIO $ runExceptT $ postAircraft ac m b

deleteAllAircraftC :: Client (Either ServantError NoContent)
deleteAllAircraftC = do
  ClientEnv m b <- ask
  liftIO $ runExceptT $ deleteAllAircrafts m b
-}
--------------------------------------------------------------------------------

{-
modelAPI :: Proxy ModelAPI
modelAPI = Proxy

getAllModels ::             Manager -> BaseUrl -> ClientM [Entity Model]
getModel     :: Int64    -> Manager -> BaseUrl -> ClientM Model

postModel    :: Model -> Manager -> BaseUrl
                -> ExceptT ServantError IO Int64
                
deleteAllModels :: Manager -> BaseUrl -> ClientM NoContent


getAllModels :<|> getModel :<|> postModel :<|> deleteAllModels =
  client modelAPI


getAllModelsC :: Client (Either ServantError [Entity Model])
getAllModelsC = do
  ClientEnv m b <- ask
  liftIO $ runExceptT $ getAllModels m b
  
getModelC :: Int64 -> Client (Either ServantError Model)
getModelC iD = do
  ClientEnv m b <- ask
  liftIO $ runExceptT $ getModel iD m b

postModelC :: Model -> Client (Either ServantError Int64)
postModelC x = do
  ClientEnv m b <- ask
  liftIO $ runExceptT $ postModel x m b

deleteAllModelsC :: Client (Either ServantError NoContent)
deleteAllModelsC = do
  ClientEnv m b <- ask
  liftIO $ runExceptT $ deleteAllModels m b
-}
