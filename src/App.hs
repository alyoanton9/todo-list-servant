{-# LANGUAGE OverloadedStrings #-}

module App
  ( taskListApp
  ) where

import Control.Monad.Reader (runReaderT)
import Servant

import Api
import Config
import Handlers

-- Convertions needed to use 'App' monad instead of 'Handler' for handlers

taskListApp :: Config -> Application
taskListApp config = serve taskListAPI $ taskListServer config

taskListServer :: Config -> Server TaskListAPI
taskListServer config = hoistServer taskListAPI (appToHandler config) taskListServerT

appToHandler :: Config -> App a -> Handler a
appToHandler config app = runReaderT (runApp app) config
