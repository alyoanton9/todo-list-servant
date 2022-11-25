{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handlers
  ( AppT(..)
  , App
  , taskListServerT
  , taskListAPI
  ) where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Database.Persist.Postgresql
import Servant

import Api
import Entity
import Config

newtype AppT m a =
  AppT
    { runApp ::
      ReaderT Config (ExceptT ServerError m) a
    }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadReader Config, MonadError ServerError
    )

type App = AppT IO

taskListAPI :: Proxy TaskListAPI
taskListAPI = Proxy

taskListServerT :: MonadIO m => ServerT TaskListAPI (AppT m)
taskListServerT = (getEntityTasks :<|> createEntityTask) :<|> taskOperations
  where
    taskOperations taskId =
           getEntityTaskById taskId
      :<|> replaceEntityTaskById taskId
      :<|> deleteEntityTaskById taskId

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
