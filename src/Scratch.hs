{-# LANGUAGE ScopedTypeVariables #-}

module Scratch where

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

-- persistent
import Database.Persist (Filter, count, delete, deleteWhere, insert,
                         replace, selectList)
       
import Database.Persist.Postgresql (fromSqlKey, runSqlPool, toSqlKey)


import           Servant                     ( (:>), (:<|>)((:<|>)), Capture
                                             , DeleteNoContent, Get
                                             , JSON, Post, Proxy(Proxy)
                                             , NoContent(..), ReqBody
                                             , ServerT, err404, throwError)
-- servant-client
import Servant.Client (BaseUrl(BaseUrl), ClientM, Scheme(Http), ServantError, client)


import Client
import Config
import Models
import qualified Db
import           Database.Persist.Types (Entity, entityVal)

import Models
import Api.Aircraft
import Ios

io1 :: IO ()
io1 = do
    putStrLn "io1"
    let
      m1 = Model "m1Code" "m1Name" 2 "m1OrdId"
      ac1 = Aircraft "SN1" $ toSqlKey 1
      
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
      (acs :: [Entity Aircraft]) <- Db.getAllAircrafts 
      let ac2 = entityVal $ head $ acs :: Aircraft

      when (ac2 /= ac1) $ do
        liftIO $ putStrLn "error"

      liftIO $ assert (ac1 == ac2) $ return ()

io1a :: IO ()
io1a = do
    putStrLn "io1"
    let
      m1 = Model "m1Code" "m1Name" 2 "m1OrdId"
      ac1 = Aircraft "SN1" $ toSqlKey 1
      
    pool <- makePool Localhost
      
    flip runSqlPool pool $ do
      deleteWhere ([] :: [Filter Aircraft])
      deleteWhere ([] :: [Filter Model])
      nAc <- count ([] :: [Filter Aircraft])
      liftIO $ putStrLn $ "nAc: " ++ show nAc
      key1 <- insert ac1
      let id1 = fromSqlKey key1
      liftIO $ putStrLn $ "id1: " ++ show id1
      nAc <- count ([] :: [Filter Aircraft])
      liftIO $ putStrLn $ "nAc: " ++ show nAc
      (acs :: [Entity Aircraft]) <- selectList [] []
      let ac2 = entityVal $ head $ acs :: Aircraft

      when (ac2 /= ac1) $ do
        liftIO $ putStrLn "error"

      liftIO $ assert (ac1 == ac2) $ return ()



io2 :: IO ()
io2 = do
  mgr <- newManager defaultManagerSettings
  port <- readPort
  let
    bu = BaseUrl Http "localhost" port ""
    m1 = Model "m1Code" "m1Name" 2 "m1OrdId"
    ac1 = Aircraft "SN1" $ toSqlKey 1
    ac2 = Aircraft "SN2" $ toSqlKey 2

  mapM_ (postAircraftIO mgr bu) [ac1, ac2]
  eAcs <- getAllAircraftsIO mgr bu

  case eAcs of
    Left err -> putStrLn $ "Error: " ++ show err
    Right acs -> putStrLn $ "acs: " ++ show acs

  void $ deleteAllAircraftIO mgr bu

{-
io3 :: IO ()
io3 = do
  x1 <- async Ios.io1
  io2
  void $ wait x1
-}

io3 :: IO ()
io3 = do
  tid <- forkIO Ios.runService
  threadDelay 100000
  io2
  killThread tid
  
  

--getAllAircraftsQuery :: ClientM [Entity Aircraft]
--getAllAircraftsQuery = getAllAircrafts


{-
io2 :: IO ()
io2 = do
  manager <- newManager defaultManagerSettings
  res <- runClientM getAllAircrafts
                    (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))

  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right acs -> putStrLn $ "acs: " ++ show acs
-}

{-
io2 :: IO ()
io2 = do
  manager <- newManager defaultManagerSettings
  res <- runExceptT (getAllAircrafts manager (BaseUrl Http "localhost" 8080 ""))

  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right acs -> putStrLn $ "acs: " ++ show acs
-}


{-
data ClientEnv
  = ClientEnv
  { manager :: Manager
  , baseUrl :: BaseUrl
  }

-- | @ClientM@ is the monad in which client functions run. Contains the
-- 'Manager' and 'BaseUrl' used for requests in the reader environment.

newtype ClientM a = ClientM { runClientM' :: ReaderT ClientEnv (ExceptT ServantError IO) a }
                    deriving ( Functor, Applicative, Monad, MonadIO, Generic
                             , MonadReader ClientEnv
                             , MonadError ServantError
                             , MonadThrow, MonadCatch
                             )

runClientM :: ClientM a -> ClientEnv -> IO (Either ServantError a)
runClientM cm env = runExceptT $ (flip runReaderT env) $ runClientM' cm
-}
