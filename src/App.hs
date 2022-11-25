{-# LANGUAGE OverloadedStrings #-}

module App
  ( runTaskList
  ) where

import Control.Monad.Reader (runReaderT)
import Database.Persist.Postgresql (runMigration, runSqlPool)
import Network.Wai.Handler.Warp (run)
import Servant

import Api
import Config
import Entity
import Handlers
import Logger

-- Initializing and running

runTaskList :: IO ()
runTaskList = do
  withConfig $ \config -> do
    app <- initialize config
    run (configPort config) app

initialize :: Config -> IO Application
initialize config = do
  let logger = katipLogger $ configLogEnv config
  runSqlPool (runMigration migrateAll) (configPool config)
  -- Add logging middleware
  return . logger $ taskListApp config

withConfig :: (Config -> IO a) -> IO a
withConfig action = do
  logEnv <- defaultLogEnv
  pool <- makePool logEnv
  let config = Config
        { configPool   = pool
        , configLogEnv = logEnv
        , configPort   = defaultPort
        }
  action config

-- Server

taskListApp :: Config -> Application
taskListApp config = serve taskListAPI $ taskListServer config

taskListServer :: Config -> Server TaskListAPI
taskListServer config = hoistServer taskListAPI (appToHandler config) taskListServerT

appToHandler :: Config -> AppT IO a -> Handler a
appToHandler config appT = Handler $ runReaderT (runApp appT) config
