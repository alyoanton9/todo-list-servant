{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( TaskListAPI
  , taskListAPI
  , taskListApp
  ) where

import Control.Monad.Except (MonadIO)
import Control.Monad.Reader (runReaderT)
import Database.Persist.Postgresql
import Servant

import Config
import Entity

-- API

type TaskListAPI
  =   "api" :> "task" :>
    (    Get '[JSON] [Entity TaskS]
    :<|> ReqBody '[JSON] TaskS :> Post '[JSON] (Entity TaskS)
    )
  :<|> "api" :> "task" :> Capture "taskId" TaskSId :>
    (    Get '[JSON] (Entity TaskS)
    :<|> ReqBody '[JSON] TaskS :> Put '[JSON] (Entity TaskS)
    :<|> Delete '[JSON] (Entity TaskS)
    )

taskListServerT :: MonadIO m => ServerT TaskListAPI (AppT m)
taskListServerT = (getEntityTasks :<|> createEntityTask) :<|> taskOperations
  where
    taskOperations taskId =
           getEntityTaskById taskId
      :<|> replaceEntityTaskById taskId
      :<|> deleteEntityTaskById taskId

-- Handlers

getEntityTasks :: MonadIO m => AppT m [Entity TaskS]
getEntityTasks = runDB $ selectList [] []

createEntityTask :: MonadIO m => TaskS -> AppT m (Entity TaskS)
createEntityTask task = do
  taskId <- runDB $ insert task
  return $ Entity taskId task

getEntityTaskById :: MonadIO m => TaskSId -> AppT m (Entity TaskS)
getEntityTaskById taskId = do
  maybeTask <- runDB $ get taskId
  case maybeTask of
    Just task -> return $ Entity taskId task
    Nothing   -> throwError err404

deleteEntityTaskById :: MonadIO m => TaskSId -> AppT m (Entity TaskS)
deleteEntityTaskById taskId = do
  entityTask <- getEntityTaskById taskId
  runDB $ delete taskId
  return $ entityTask

replaceEntityTaskById :: MonadIO m => TaskSId -> TaskS -> AppT m (Entity TaskS)
replaceEntityTaskById taskId task = do
  _ <- getEntityTaskById taskId
  runDB $ replace taskId task
  return $ Entity taskId task

-- Server

taskListAPI :: Proxy TaskListAPI
taskListAPI = Proxy

taskListApp :: Config -> Application
taskListApp config = serve taskListAPI $ taskListServer config

taskListServer :: Config -> Server TaskListAPI
taskListServer config = hoistServer taskListAPI (appToHandler config) taskListServerT

appToHandler :: Config -> AppT IO a -> Handler a
appToHandler config appT = Handler $ runReaderT (runApp appT) config
