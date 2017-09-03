{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Aircraft where

-- import           Control.Monad.Except ()
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (App (..), Config (..))
import           Models

type AircraftAPI =
         "aircrafts" :> Get '[JSON] [Entity Aircraft]
    :<|> "aircrafts" :> Capture "name" String :> Get '[JSON] (Entity Aircraft)
    :<|> "aircrafts" :> ReqBody '[JSON] Aircraft :> Post '[JSON] Int64

-- | The server that runs the AircraftAPI
aircraftServer :: ServerT AircraftAPI App
aircraftServer = allAircrafts :<|> singleAircraft :<|> createAircraft

-- | Returns all aircrafts in the database.
allAircrafts :: App [Entity Aircraft]
allAircrafts =
    runDb (selectList [] [])

-- | Returns a aircraft by name or throws a 404 error.
singleAircraft :: String -> App (Entity Aircraft)
singleAircraft str = do
    maybeAircraft <- runDb (selectFirst [AircraftSerialNumber ==. str] [])
    case maybeAircraft of
         Nothing ->
            throwError err404
         Just person ->
            return person

-- | Creates a aircraft in the database.
createAircraft :: Aircraft -> App Int64
createAircraft p = do
    newAircraft <- runDb (insert (Aircraft (aircraftSerialNumber p)
                                           (aircraftNumEngines p)))
    return $ fromSqlKey newAircraft

-- | Generates JavaScript to query the Aircraft API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy AircraftAPI) vanillaJS "./assets/api.js"
