module Main where

-- base
import Data.Monoid ((<>))
import Control.Exception (throw)

-- bytestring
import qualified Data.ByteString as BS
-- directory
import System.Directory (getCurrentDirectory)

-- yaml
import Data.Yaml (decodeEither')

import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           Api.Aircraft                (generateJavaScript)

import           Config                      ( Config(Config, getEnv, getPool)
                                             , Environment(Localhost)
                                             , makePool, setLogger)
                 
import           Models                      (doMigrations)
import           Safe                        (readMay)


import Config

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do

  currDir <- getCurrentDirectory
  putStrLn $ "currDir: " <> currDir

  appConfigBS <- BS.readFile "config/application.yml"
  let appConfig = either throw id $ decodeEither' appConfigBS :: AppConfig

  putStrLn $ "appConfig: " ++ (show appConfig)
  
  
  env  <- lookupSetting "ENV" Localhost
  port <- lookupSetting "PORT" 8081
  pool <- makePool env
    
  let cfg = Config { getPool = pool, getEnv = env }
      logger = setLogger env
        
  runSqlPool doMigrations pool
  generateJavaScript
  run port $ logger $ app cfg

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]
