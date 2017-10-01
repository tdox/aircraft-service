module Client where

-- base
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (void, when)
import Data.Int (Int64)
-- import GHC.Generics ()
-- import Data.Proxy (Proxy(Proxy))

-- assert
import Control.Exception.Assert (assert)

-- async
-- import Control.Concurrent.Async ()

-- exceptions
-- import Control.Monad.Catch ()

-- http-client
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)

-- mtl
import Control.Monad.Except (ExceptT, runExceptT)
-- import Control.Monad.Reader
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)

import Database.Persist.Postgresql (fromSqlKey, runSqlPool)


import           Servant                     ( (:>), (:<|>)((:<|>)), Capture
                                             , DeleteNoContent, Get
                                             , JSON, Post, Proxy(Proxy)
                                             , NoContent(..), ReqBody
                                             , ServerT, err404, throwError)
-- servant-client
import Servant.Client (BaseUrl(BaseUrl), ClientM, Scheme(Http), ServantError, client)


import Config
import Models
import qualified Db
import           Database.Persist.Types (Entity, entityVal)

import Models
import Api.Aircraft
import Ios


aircraftAPI :: Proxy AircraftAPI
aircraftAPI = Proxy

-- getAllAircrafts :: ClientM [Entity Aircraft]
getAllAircrafts :: Manager -> BaseUrl -> ClientM [Entity Aircraft]
getAircraft :: Int64 -> Manager -> BaseUrl -> ClientM Aircraft
postAircraft :: Aircraft -> Manager -> BaseUrl -> ExceptT ServantError IO Int64
deleteAll :: Manager -> BaseUrl -> ClientM NoContent


getAllAircrafts :<|> getAircraft :<|> postAircraft :<|> deleteAll = client aircraftAPI

getAllAircraftsIO :: Manager -> BaseUrl -> IO (Either ServantError [Entity Aircraft])
getAllAircraftsIO mgr bu = runExceptT $ getAllAircrafts mgr bu

getAircraftIO :: Manager -> BaseUrl -> Int64 -> IO (Either ServantError Aircraft)
getAircraftIO mgr bu iD = runExceptT $ getAircraft iD mgr bu

postAircraftIO :: Manager -> BaseUrl -> Aircraft -> IO (Either ServantError Int64)
postAircraftIO m b ac = runExceptT $ postAircraft ac m b

deleteAllIO :: Manager -> BaseUrl -> IO (Either ServantError NoContent)
deleteAllIO m b = runExceptT $ deleteAll m b

