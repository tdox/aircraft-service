{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Aircraft where

import Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader        (ReaderT, ask, runReaderT)
import           Data.Int                    (Int64)

import           Database.Persist.Postgresql (Entity(..), Key, Filter, deleteWhere
                                             , fromSqlKey, get
                                             , insert
                                             , selectFirst, selectList, toSqlKey, (==.))

import           Network.Wai                 (Application)

import           Servant                     ( (:>), (:<|>)((:<|>)), Capture
                                             , DeleteNoContent, Get
                                             , JSON, Post, Proxy(Proxy)
                                             , NoContent(..), ReqBody
                                             , ServerT, err404, throwError)

-- import Servant.Client
                 
import           Servant.JS                  ( vanillaJS, writeJSForAPI)

import           Config                      ( App, Config(getEnv)
                                             , Environment(Localhost))

import Db (runDb)
                 
import           Models                      ( Aircraft(Aircraft
                                                       , aircraftSerialNumber
                                                       , aircraftNumEngines)
                                             )
import qualified Models  as M

--------------------------------------------------------------------------------

type AircraftAPI =
         "aircrafts" :> Get '[JSON] [Entity Aircraft]
    :<|> "aircrafts" :> Capture "id" Int64 :> Get '[JSON] Aircraft
    :<|> "aircrafts" :> ReqBody '[JSON] Aircraft :> Post '[JSON] Int64
    :<|> "aircrafts" :> "deleteAll" :> DeleteNoContent '[JSON] NoContent

-- | The server that runs the AircraftAPI
aircraftServer :: ServerT AircraftAPI App
aircraftServer = allAircrafts
            :<|> singleAircraft
            :<|> createAircraft
            :<|> deleteAllAircraft

-- | Returns all aircrafts in the database.
allAircrafts :: App [Entity Aircraft]
allAircrafts =
    runDb (selectList [] [])

-- | Returns an aircraft by serialNumber or throws a 404 error.
singleAircraft :: Int64 -> App Aircraft
singleAircraft idInt = do
    let (key :: Key Aircraft) = toSqlKey idInt
    maybeAircraft <- runDb (get key)
    case maybeAircraft of
         Nothing -> throwError err404
         Just ac -> return ac

-- | Returns an aircraft by serialNumber or throws a 404 error.
aircraftBySerialNumber :: String -> App (Entity Aircraft)
aircraftBySerialNumber str = do
    maybeAircraft <- runDb (selectFirst [M.AircraftSerialNumber ==. str] [])
    case maybeAircraft of
         Nothing ->
            throwError err404
         Just person ->
            return person


-- | Creates an aircraft in the database.
createAircraft :: Aircraft -> App Int64
createAircraft ac = do
    newAircraft <- runDb $ insert ac -- Aircraft (aircraftSerialNumber ac)
                                                -- (aircraftNumEngines ac)
    return $ fromSqlKey newAircraft


-- | Delete all aircraft, but only if env is Localhost
deleteAllAircraft :: App NoContent
deleteAllAircraft = do
--  liftIO $ print "deleteAllAircraft"
  conf <- ask
  case getEnv conf of
    Localhost -> do
      runDb $ deleteWhere  {- NoContent -- -} ([] :: [Filter Aircraft])
      return NoContent
    _ -> return NoContent


-- | Generates JavaScript to query the Aircraft API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy AircraftAPI) vanillaJS "./assets/api.js"


