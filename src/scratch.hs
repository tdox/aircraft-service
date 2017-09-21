import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)

import           Database.Persist.Postgresql (fromSqlKey, runSqlPool)

import Config
import Models
import Db

io1 :: IO ()
io1 = do
    putStrLn "io1"
    let
      ac1 = Aircraft "SN1" 4
      
    pool <- makePool Localhost
      
    flip runSqlPool pool $ do
      key1 <- insertAircraft ac1
      let id1 = fromSqlKey key1
      liftIO $ putStrLn $ "id1: " ++ show id1
      
