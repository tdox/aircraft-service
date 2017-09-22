
{-# LANGUAGE ScopedTypeVariables #-}

-- base
import Control.Monad (when)

-- assert
import Control.Exception.Assert (assert)

import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)

import           Database.Persist.Postgresql (fromSqlKey, runSqlPool)

import Config
import Models
import Db
import           Database.Persist.Types (Entity, entityVal)

io1 :: IO ()
io1 = do
    putStrLn "io1"
    let
      ac1 = Aircraft "SN1" 4
      
    pool <- makePool Localhost
      
    flip runSqlPool pool $ do
      deleteAllAircraft
      nAc <- countAircraft
      liftIO $ putStrLn $ "nAc: " ++ show nAc
      key1 <- insertAircraft ac1
      let id1 = fromSqlKey key1
      liftIO $ putStrLn $ "id1: " ++ show id1
      nAc <- countAircraft
      liftIO $ putStrLn $ "nAc: " ++ show nAc
      (acs :: [Entity Aircraft]) <- allAircrafts 
      let ac2 = entityVal $ head $ acs :: Aircraft

      when (ac2 /= ac1) $ do
        liftIO $ putStrLn "error"

      liftIO $ assert (ac1 == ac2) $ return ()


      
