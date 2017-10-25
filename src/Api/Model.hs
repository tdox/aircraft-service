{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Model where

import Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader        (ReaderT, ask, runReaderT)
import           Data.Int                    (Int64)

import           Database.Persist.Postgresql (Entity(..), Key, Filter
                                             , deleteWhere
                                             , fromSqlKey, get
                                             , insert
                                             , selectFirst, selectList
                                             , toSqlKey, (==.))

import           Network.Wai                 (Application)

import           Servant                     ( (:>), (:<|>)((:<|>)), Capture
                                             , DeleteNoContent, Get
                                             , JSON, Post, Proxy(Proxy)
                                             , NoContent(..), ReqBody
                                             , ServerT, err404, throwError)

                 
import           Servant.JS                  ( vanillaJS, writeJSForAPI)

import           Config                      ( App, Config(getEnv)
                                             , Environment(Localhost))

import Db (runDb)
                 
import           Models                      ( Model

                                             )
import qualified Models  as M

--------------------------------------------------------------------------------

type ModelAPI =
         "models" :> Get '[JSON] [Entity Model]
    :<|> "models" :> Capture "id" Int64 :> Get '[JSON] Model
    :<|> "models" :> ReqBody '[JSON] Model :> Post '[JSON] Int64
    :<|> "models" :> "deleteAll" :> DeleteNoContent '[JSON] NoContent

-- | The server that runs the ModelAPI
modelServer :: ServerT ModelAPI App
modelServer = allModels
            :<|> singleModel
            :<|> createModel
            :<|> deleteAllModels

-- | Returns all models in the database.
allModels :: App [Entity Model]
allModels =
    runDb (selectList [] [])

-- | Returns a model by id or throws a 404 error.
singleModel :: Int64 -> App Model
singleModel idInt = do
    let (key :: Key Model) = toSqlKey idInt
    maybeModel <- runDb (get key)
    case maybeModel of
         Nothing -> throwError err404
         Just ac -> return ac


-- | Creates a model in the database.
createModel :: Model -> App Int64
createModel ac = do
    newModel <- runDb $ insert ac
    return $ fromSqlKey newModel


-- | Delete all models, but only if env is Localhost
deleteAllModels :: App NoContent
deleteAllModels = do
--  liftIO $ print "deleteAllModel"
  conf <- ask
  case getEnv conf of
    Localhost -> do
      runDb $ deleteWhere ([] :: [Filter Model])
      return NoContent
    _ -> return NoContent


-- | Generates JavaScript to query the Model API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy ModelAPI) vanillaJS "./assets/api.js"


