
{-# LANGUAGE ScopedTypeVariables #-}

-- base
import Control.Monad (when)
-- import Data.Proxy (Proxy(Proxy))

-- assert
import Control.Exception.Assert (assert)

-- http-client
import Network.HTTP.Client (defaultManagerSettings, newManager)

import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)

import           Database.Persist.Postgresql (fromSqlKey, runSqlPool)


import           Servant                     ( (:>), (:<|>)((:<|>)), Capture
                                             , DeleteNoContent, Get
                                             , JSON, Post, Proxy(Proxy)
                                             , NoContent(..), ReqBody
                                             , ServerT, err404, throwError)

import Servant.Client -- (ClientM, runClientM)
--import Servant.Common.Req (runClientM)

import Config
import Models
import qualified Db
import           Database.Persist.Types (Entity, entityVal)

import Models
import Api.Aircraft

io1 :: IO ()
io1 = do
    putStrLn "io1"
    let
      ac1 = Aircraft "SN1" 4
      
    pool <- makePool Localhost
      
    flip runSqlPool pool $ do
      Db.deleteAllAircraft
      nAc <- Db.countAircraft
      liftIO $ putStrLn $ "nAc: " ++ show nAc
      key1 <- Db.insertAircraft ac1
      let id1 = fromSqlKey key1
      liftIO $ putStrLn $ "id1: " ++ show id1
      nAc <- Db.countAircraft
      liftIO $ putStrLn $ "nAc: " ++ show nAc
      (acs :: [Entity Aircraft]) <- Db.allAircrafts 
      let ac2 = entityVal $ head $ acs :: Aircraft

      when (ac2 /= ac1) $ do
        liftIO $ putStrLn "error"

      liftIO $ assert (ac1 == ac2) $ return ()


      
aircraftAPI :: Proxy AircraftAPI
aircraftAPI = Proxy

-- getAllAircrafts :: ClientM [Entity Aircraft]

getAllAircrafts :<|> getAircraft :<|> postAircraft :<|> deleteAll = client aircraftAPI

--getAllAircraftsQuery :: ClientM [Entity Aircraft]
--getAllAircraftsQuery = getAllAircrafts

io2 :: IO ()
io2 = do
  manager <- newManager defaultManagerSettings
  res <- runClientM getAllAircrafts (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))

  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right acs -> putStrLn $ "acs: " ++ show acs
