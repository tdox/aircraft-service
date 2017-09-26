{-# LANGUAGE ScopedTypeVariables #-}

module Ios where


-- base
import Data.Monoid ((<>))
import Control.Exception (throw)

-- bytestring
import qualified Data.ByteString as BS
-- directory
import System.Directory (getCurrentDirectory)

-- yaml
import Data.Yaml (decodeEither')

import           Database.Persist.Postgresql (ConnectionPool, runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           Api.Aircraft                (generateJavaScript)

import           Config                      ( Config(Config, getEnv, getPool)
                                             , Environment(Localhost)
                                             , makePool, setLogger)
                 
import           Safe                        (readMay)
import           Data.Text                   (unpack)

import           Db                         (doMigrations)

import Config

--------------------------------------------------------------------------------


-- | The 'main' function gathers the required environment information and
-- initializes the application.
io1 :: IO ()
io1 = do

  (cfg :: Config) <- readConfig
  
  let
    logger = setLogger $ getEnv cfg
    pool = getPool cfg :: ConnectionPool
    port = cPort cfg
    
        
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

-- | read the configuration from a file
readConfig :: IO Config
readConfig = do
  currDir <- getCurrentDirectory
  putStrLn $ "currDir: " <> currDir

  appConfigBS <- BS.readFile "config/application.yml"
  let appConfig = either throw id $ decodeEither' appConfigBS :: AppConfig

  putStrLn $ "appConfig: " ++ (show appConfig)

  let
    serverSettings = server appConfig
    defaultPort = port serverSettings

    defaultEnv = readEnv $ env serverSettings :: Environment
  
  
  env  <- lookupSetting "ENV" defaultEnv
  port <- lookupSetting "PORT" defaultPort
  pool <- makePool env
    
  let cfg = Config { getPool = pool, getEnv = env, cPort = port }

  return cfg
  
